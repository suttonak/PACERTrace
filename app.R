# PACERTrace v1.0.0

library(shiny)
library(bslib)  # NEW: Modern UI theme
library(shinyWidgets)  # NEW: Better input controls
library(hdf5r)
library(magick)
library(dplyr)
library(ggplot2)
library(MASS)
library(sf)
library(shinyjs)
library(sp)
library(tidyr)
library(viridis)
library(purrr)
library(stringr)
library(zoo)

options(shiny.maxRequestSize = 4000 * 1024^2)
roi_colors <- c("blue", "green", "purple", "orange", "red", 
                "cyan", "magenta", "yellow", "brown", "pink")
add_roi_overlays <- function(p, selected_roi_names, roi_df, roi_colors, add_labels = TRUE) {
  if (is.null(selected_roi_names) || length(selected_roi_names) == 0) return(p)
  if (is.null(roi_df) || nrow(roi_df) == 0) return(p)
  
  # draw each requested ROI as dashed outline; color-cycled
  for (i in seq_along(selected_roi_names)) {
    nm <- selected_roi_names[i]
    poly <- roi_df[roi_df$Name == nm, c("x","y")]
    if (nrow(poly) >= 3) {
      col_idx <- (i - 1) %% length(roi_colors) + 1
      col <- roi_colors[col_idx]
      
      p <- p +
        ggplot2::geom_polygon(
          data = poly, ggplot2::aes(x = x, y = y),
          inherit.aes = FALSE, fill = NA, color = col,
          linewidth = 0.9, linetype = "dashed"
        )
      
      if (add_labels) {
        p <- p + ggplot2::annotate(
          "text", x = mean(poly$x), y = mean(poly$y),
          label = nm, color = col, fontface = "bold", size = 3
        )
      }
    }
  }
  p
}

step_size <- 20

# Utility functions
extract_frame <- function(video_path, time_sec) {
  ffmpeg_path <- "/opt/homebrew/bin/ffmpeg"
  tmpfile <- tempfile(fileext = ".png")
  cmd <- sprintf(
    '"%s" -y -ss %.2f -i "%s" -frames:v 1 -f image2 "%s"',
    ffmpeg_path, time_sec, video_path, tmpfile
  )
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  tmpfile
}

extract_fps <- function(video_path) {
  ffprobe_path <- "/opt/homebrew/bin/ffprobe"
  cmd <- sprintf('"%s" -v error -select_streams v:0 -show_entries stream=r_frame_rate -of default=nokey=1:noprint_wrappers=1 "%s"', 
                 ffprobe_path, video_path)
  fps_raw <- system(cmd, intern = TRUE)
  fps_raw <- trimws(fps_raw)
  if (grepl("/", fps_raw)) {
    parts <- unlist(strsplit(fps_raw, "/"))
    fps <- as.numeric(parts[1]) / as.numeric(parts[2])
  } else {
    fps <- as.numeric(fps_raw)
  }
  if (is.na(fps) || fps <= 0) stop("Failed to parse valid FPS.")
  return(fps)
}

# Function to find nearby ROI edges for snapping
find_nearby_edges <- function(click_x, click_y, roi_data, snap_distance = 10) {
  if (nrow(roi_data) == 0) return(NULL)
  
  roi_names <- unique(roi_data$Name)
  closest_point <- NULL
  min_distance <- Inf
  
  for (roi_name in roi_names) {
    roi_points <- roi_data[roi_data$Name == roi_name, ]
    if (nrow(roi_points) >= 2) {
      # Check distance to each vertex
      for (i in 1:nrow(roi_points)) {
        dist <- sqrt((click_x - roi_points$x[i])^2 + (click_y - roi_points$y[i])^2)
        if (dist < min_distance && dist <= snap_distance) {
          min_distance <- dist
          closest_point <- list(x = roi_points$x[i], y = roi_points$y[i], 
                                roi = roi_name, distance = dist)
        }
      }
      
      # Check distance to each edge (midpoint)
      for (i in 1:nrow(roi_points)) {
        next_i <- ifelse(i == nrow(roi_points), 1, i + 1)
        edge_x <- (roi_points$x[i] + roi_points$x[next_i]) / 2
        edge_y <- (roi_points$y[i] + roi_points$y[next_i]) / 2
        
        dist <- sqrt((click_x - edge_x)^2 + (click_y - edge_y)^2)
        if (dist < min_distance && dist <= snap_distance) {
          min_distance <- dist
          closest_point <- list(x = edge_x, y = edge_y, 
                                roi = roi_name, distance = dist, type = "edge")
        }
      }
    }
  }
  
  return(closest_point)
}

# Function to suggest shared edges between ROIs
suggest_shared_edge <- function(current_polygon, roi_data) {
  if (nrow(current_polygon) < 2 || nrow(roi_data) == 0) return(NULL)
  
  # Find the last edge of the current polygon
  last_point <- current_polygon[nrow(current_polygon), ]
  second_last_point <- current_polygon[nrow(current_polygon) - 1, ]
  
  # Check if this edge is close to any existing ROI edges
  roi_names <- unique(roi_data$Name)
  for (roi_name in roi_names) {
    roi_points <- roi_data[roi_data$Name == roi_name, ]
    if (nrow(roi_points) >= 2) {
      for (i in 1:(nrow(roi_points))) {
        next_i <- ifelse(i == nrow(roi_points), 1, i + 1)
        
        roi_edge_start <- roi_points[i, ]
        roi_edge_end <- roi_points[next_i, ]
        
        dist_to_start <- sqrt((last_point$x - roi_edge_start$x)^2 + (last_point$y - roi_edge_start$y)^2)
        dist_to_end <- sqrt((second_last_point$x - roi_edge_end$x)^2 + (second_last_point$y - roi_edge_end$y)^2)
        
        if (dist_to_start < 20 && dist_to_end < 20) {
          return(list(
            roi = roi_name,
            suggested_points = list(
              list(x = roi_edge_start$x, y = roi_edge_start$y),
              list(x = roi_edge_end$x, y = roi_edge_end$y)
            )
          ))
        }
      }
    }
  }
  
  return(NULL)
}

# FIXED: Improved merge_tracks function
merge_tracks <- function(h5_path, node) {
  h5 <- H5File$new(h5_path, mode = "r")
  
  if (!"tracks" %in% names(h5)) {
    h5$close_all()
    stop("No 'tracks' dataset found in H5 file")
  }
  
  tracks <- h5[["tracks"]]$read()
  
  if ("node_names" %in% names(h5)) {
    node_names <- h5[["node_names"]]$read()
    if (is.list(node_names)) {
      node_names <- sapply(node_names, function(x) {
        if (is.raw(x)) {
          rawToChar(x[x != 0])
        } else {
          as.character(x)
        }
      })
    } else {
      node_names <- sapply(node_names, function(x) gsub("\\x00", "", x))
    }
    node_names <- trimws(node_names)
  } else {
    h5$close_all()
    stop("No 'node_names' dataset found in H5 file")
  }
  
  h5$close_all()
  
  cat("Tracks shape:", dim(tracks), "\n")
  cat("Node names:", node_names, "\n")
  cat("Looking for node:", node, "\n")
  
  n_frames <- dim(tracks)[1]
  track_dims <- length(dim(tracks))
  
  if (track_dims == 4) {
    n_nodes <- dim(tracks)[2]
    n_coords <- dim(tracks)[3]
    n_instances <- dim(tracks)[4]
    cat("4D structure detected: [frames, nodes, coordinates, instances]\n")
  } else if (track_dims == 3) {
    n_nodes <- dim(tracks)[2]
    n_coords <- dim(tracks)[3]
    n_instances <- 1
    cat("3D structure detected: [frames, nodes, coordinates]\n")
  } else {
    stop(paste("Unexpected tracks array structure with", track_dims, "dimensions"))
  }
  
  if (!(node %in% node_names)) {
    warning(paste("Node", node, "not found in node_names"))
    return(matrix(NA, nrow = n_frames, ncol = 2))
  }
  
  node_idx <- which(node_names == node)
  merged <- matrix(NA, nrow = n_frames, ncol = 2)
  
  if (track_dims == 4) {
    cat("Processing 4D structure for node", node, "(index", node_idx, ")\n")
    for (f in 1:n_frames) {
      for (t in 1:n_instances) {
        coords <- tracks[f, node_idx, 1:2, t]
        if (!any(is.na(coords)) && !any(is.infinite(coords))) {
          merged[f, ] <- coords
          break
        }
      }
    }
  } else if (track_dims == 3) {
    cat("Processing 3D structure for node", node, "(index", node_idx, ")\n")
    for (f in 1:n_frames) {
      coords <- tracks[f, node_idx, 1:2]
      if (!any(is.na(coords)) && !any(is.infinite(coords))) {
        merged[f, ] <- coords
      }
    }
  }
  
  valid_coords <- sum(!is.na(merged[,1]))
  cat("Valid coordinates for", node, ":", valid_coords, "out of", n_frames, "\n")
  
  return(merged)
}

debug_roi_coverage <- function(h5_path, node, roi_def, fps) {
  coords <- merge_tracks(h5_path, node)
  valid_indices <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
  
  if (length(valid_indices) == 0) {
    return(list(
      total_frames = nrow(coords),
      valid_frames = 0,
      roi_coverage = data.frame()
    ))
  }
  
  frame_roi_status <- data.frame(
    Frame = valid_indices,
    Time_sec = valid_indices / fps,
    InAnyROI = FALSE
  )
  
  for (roi_name in names(roi_def)) {
    poly <- roi_def[[roi_name]]
    if (nrow(poly) >= 3) {
      inside <- point.in.polygon(
        coords[valid_indices, 1], 
        coords[valid_indices, 2], 
        poly[,1], 
        poly[,2]
      ) > 0
      frame_roi_status[[roi_name]] <- inside
      frame_roi_status$InAnyROI <- frame_roi_status$InAnyROI | inside
    }
  }
  
  coverage_summary <- list(
    total_frames = nrow(coords),
    valid_frames = length(valid_indices),
    frames_in_any_roi = sum(frame_roi_status$InAnyROI),
    frames_outside_all_rois = sum(!frame_roi_status$InAnyROI),
    percent_outside_rois = round(sum(!frame_roi_status$InAnyROI) / length(valid_indices) * 100, 2),
    time_outside_rois = sum(!frame_roi_status$InAnyROI) / fps
  )
  
  return(list(
    summary = coverage_summary,
    frame_details = frame_roi_status
  ))
}

generate_frame_roi_data <- function(h5_path, roi_def, fps) {
  h5 <- H5File$new(h5_path, mode = "r")
  
  if ("node_names" %in% names(h5)) {
    node_names <- h5[["node_names"]]$read()
    if (is.list(node_names)) {
      node_names <- sapply(node_names, function(x) {
        if (is.raw(x)) {
          rawToChar(x[x != 0])
        } else {
          as.character(x)
        }
      })
    } else {
      node_names <- sapply(node_names, function(x) gsub("\\x00", "", x))
    }
    node_names <- trimws(node_names)
  } else {
    h5$close_all()
    stop("No node_names found in H5 file")
  }
  
  tracks <- h5[["tracks"]]$read()
  n_frames <- dim(tracks)[1]
  h5$close_all()
  
  all_frame_data <- data.frame()
  
  for (node in node_names) {
    cat("Processing frame data for node:", node, "\n")
    coords <- merge_tracks(h5_path, node)
    
    frame_data <- data.frame(
      Frame = 1:n_frames,
      Time_sec = round((1:n_frames) / fps, 3),
      Node = node,
      X_coord = coords[,1],
      Y_coord = coords[,2],
      Valid = !is.na(coords[,1]) & !is.na(coords[,2])
    )
    
    for (roi_name in names(roi_def)) {
      poly <- roi_def[[roi_name]]
      if (nrow(poly) >= 3) {
        inside <- rep(FALSE, n_frames)
        valid_indices <- which(frame_data$Valid)
        if (length(valid_indices) > 0) {
          inside[valid_indices] <- point.in.polygon(
            coords[valid_indices, 1], 
            coords[valid_indices, 2], 
            poly[,1], 
            poly[,2]
          ) > 0
        }
        frame_data[[paste0("ROI_", roi_name)]] <- inside
      } else {
        frame_data[[paste0("ROI_", roi_name)]] <- FALSE
      }
    }
    
    all_frame_data <- rbind(all_frame_data, frame_data)
  }
  
  return(all_frame_data)
}

generate_outside_roi_heatmap <- function(h5_path, node, roi_def, fps) {
  coords <- merge_tracks(h5_path, node)
  valid_indices <- which(!is.na(coords[,1]) & !is.na(coords[,2]))
  
  if (length(valid_indices) == 0) {
    return(list(
      summary = list(
        total_frames = nrow(coords),
        valid_frames = 0,
        outside_roi_frames = 0,
        outside_roi_percent = 0,
        missing_percent = 100,
        inside_roi_percent = 0
      ),
      outside_roi_coords = data.frame(x = numeric(0), y = numeric(0))
    ))
  }
  
  outside_roi_mask <- rep(TRUE, length(valid_indices))
  
  for (roi_name in names(roi_def)) {
    poly <- roi_def[[roi_name]]
    if (nrow(poly) >= 3) {
      inside <- point.in.polygon(
        coords[valid_indices, 1], 
        coords[valid_indices, 2], 
        poly[,1], 
        poly[,2]
      ) > 0
      outside_roi_mask <- outside_roi_mask & !inside
    }
  }
  
  outside_roi_indices <- valid_indices[outside_roi_mask]
  outside_roi_coords <- data.frame(
    x = coords[outside_roi_indices, 1],
    y = coords[outside_roi_indices, 2]
  )
  
  total_frames <- nrow(coords)
  valid_frames <- length(valid_indices)
  outside_roi_frames <- length(outside_roi_indices)
  inside_roi_frames <- valid_frames - outside_roi_frames
  missing_frames <- total_frames - valid_frames
  
  summary_stats <- list(
    total_frames = total_frames,
    valid_frames = valid_frames,
    outside_roi_frames = outside_roi_frames,
    inside_roi_frames = inside_roi_frames,
    missing_frames = missing_frames,
    outside_roi_percent = round(outside_roi_frames / valid_frames * 100, 2),
    inside_roi_percent = round(inside_roi_frames / valid_frames * 100, 2),
    missing_percent = round(missing_frames / total_frames * 100, 2)
  )
  
  return(list(
    summary = summary_stats,
    outside_roi_coords = outside_roi_coords
  ))
}

# CRITICAL FIX: Enhanced compute_roi_occupancy with comprehensive validation
compute_roi_occupancy <- function(coords, roi_def, fps, bin_size) {
  # Validate all inputs
  if (is.null(coords) || !is.matrix(coords) || ncol(coords) < 2) {
    warning("Invalid coords matrix")
    return(data.frame(
      Bin = integer(0),
      ROI = character(0),
      Frames = integer(0),
      Time_sec = numeric(0),
      Bin_Start = numeric(0),
      Bin_End = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  if (is.null(fps) || is.na(fps) || length(fps) == 0 || fps <= 0) {
    warning("Invalid FPS value")
    return(data.frame(
      Bin = integer(0),
      ROI = character(0),
      Frames = integer(0),
      Time_sec = numeric(0),
      Bin_Start = numeric(0),
      Bin_End = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  if (is.null(bin_size) || is.na(bin_size) || length(bin_size) == 0 || bin_size <= 0) {
    warning("Invalid bin_size value")
    return(data.frame(
      Bin = integer(0),
      ROI = character(0),
      Frames = integer(0),
      Time_sec = numeric(0),
      Bin_Start = numeric(0),
      Bin_End = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  if (is.null(roi_def) || length(roi_def) == 0) {
    warning("No ROIs defined")
    return(data.frame(
      Bin = integer(0),
      ROI = character(0),
      Frames = integer(0),
      Time_sec = numeric(0),
      Bin_Start = numeric(0),
      Bin_End = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Check valid coordinates
  valid_coords <- !is.na(coords[,1]) & !is.na(coords[,2])
  n_valid <- sum(valid_coords)
  
  cat("Computing ROI occupancy - Valid coords:", n_valid, "out of", nrow(coords), "\n")
  
  if (n_valid == 0 || nrow(coords) == 0) {
    warning("No valid coordinates for ROI occupancy calculation")
    return(data.frame(
      Bin = integer(0),
      ROI = character(0),
      Frames = integer(0),
      Time_sec = numeric(0),
      Bin_Start = numeric(0),
      Bin_End = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  frame_time <- (1:nrow(coords)) / fps
  bin_group <- floor(frame_time / bin_size)
  
  results <- data.frame(
    Frame = 1:nrow(coords), 
    Time = frame_time, 
    Bin = bin_group,
    Valid = valid_coords,
    stringsAsFactors = FALSE
  )
  
  # Compute ROI occupancy for each ROI
  roi_cols <- character(0)  # Initialize empty
  
  for (roi_name in names(roi_def)) {
    poly <- roi_def[[roi_name]]
    if (nrow(poly) >= 3) {
      inside <- rep(FALSE, nrow(coords))
      if (n_valid > 0) {
        valid_indices <- which(valid_coords)
        inside[valid_indices] <- point.in.polygon(
          coords[valid_indices, 1], 
          coords[valid_indices, 2], 
          poly[,1], 
          poly[,2]
        ) > 0
      }
      results[[roi_name]] <- inside
      roi_cols <- c(roi_cols, roi_name)  # Add to list of ROI columns
    }
  }
  
  # CRITICAL: Check if we have any valid ROI columns before proceeding
  if (length(roi_cols) == 0) {
    warning("No valid ROI polygons found (need at least 3 points)")
    return(data.frame(
      Bin = integer(0),
      ROI = character(0),
      Frames = integer(0),
      Time_sec = numeric(0),
      Bin_Start = numeric(0),
      Bin_End = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  # Summarize by bin - now we know roi_cols has content
  summary_results <- results %>%
    group_by(Bin) %>%
    summarise(
      Total_Frames = n(),
      Valid_Frames = sum(Valid),
      across(all_of(roi_cols), ~sum(.x, na.rm = TRUE)),
      .groups = 'drop'
    ) %>%
    pivot_longer(
      cols = all_of(roi_cols), 
      names_to = "ROI", 
      values_to = "Frames"
    ) %>%
    mutate(
      Time_sec = Frames / fps,
      Bin_Start = Bin * bin_size,
      Bin_End = (Bin + 1) * bin_size
    )
  
  return(summary_results)
}

# Continue with rest of the functions...
interp_with_maxgap <- function(v, max_gap = 3) {
  if (all(is.na(v))) return(v)
  n <- length(v)
  ok <- !is.na(v)
  if (sum(ok) < 2) return(v)
  if ("zoo" %in% .packages()) {
    return(zoo::na.approx(v, na.rm = FALSE, maxgap = max_gap))
  }
  idx <- which(ok)
  for (k in seq_along(idx) - 1) {
    if (k == 0) next
    i1 <- idx[k]
    i2 <- idx[k + 1]
    gap <- i2 - i1 - 1
    if (gap > 0 && gap <= max_gap) {
      v[(i1 + 1):(i2 - 1)] <- v[i1] + (v[i2] - v[i1]) * (1:gap) / (gap + 1)
    }
  }
  v
}

compute_distance_series <- function(coords, fps, px_per_cm = NA, max_interp_gap = 3) {
  x <- coords[,1]; y <- coords[,2]
  xi <- interp_with_maxgap(x, max_interp_gap)
  yi <- interp_with_maxgap(y, max_interp_gap)
  
  dx <- c(NA, diff(xi))
  dy <- c(NA, diff(yi))
  step_px <- sqrt(dx^2 + dy^2)
  step_px[is.na(dx) | is.na(dy)] <- NA
  
  speed_px_s <- step_px * fps
  cum_px <- cumsum(replace(step_px, is.na(step_px), 0))
  
  if (!is.na(px_per_cm) && px_per_cm > 0) {
    step_cm <- step_px / px_per_cm
    speed_cm_s <- speed_px_s / px_per_cm
    cum_cm <- cum_px / px_per_cm
  } else {
    step_cm <- rep(NA_real_, length(step_px))
    speed_cm_s <- rep(NA_real_, length(step_px))
    cum_cm <- rep(NA_real_, length(step_px))
  }
  
  list(
    step_px = step_px,
    speed_px_s = speed_px_s,
    cum_px = cum_px,
    step_cm = step_cm,
    speed_cm_s = speed_cm_s,
    cum_cm = cum_cm
  )
}

bin_distance_speed <- function(time_sec, step_px, step_cm, speed_cm_s, fps, bin_size) {
  bin <- floor(time_sec / bin_size)
  df <- data.frame(bin = bin, step_px = step_px, step_cm = step_cm, speed_cm_s = speed_cm_s)
  agg <- df %>%
    group_by(bin) %>%
    summarise(
      Distance_px = sum(step_px, na.rm = TRUE),
      Distance_cm = if (all(is.na(step_cm))) NA_real_ else sum(step_cm, na.rm = TRUE),
      MeanSpeed_cm_s = if (all(is.na(speed_cm_s))) NA_real_ else mean(speed_cm_s, na.rm = TRUE),
      Bin_Start = unique(bin) * bin_size,
      Bin_End = (unique(bin) + 1) * bin_size,
      .groups = "drop"
    ) %>%
    arrange(bin)
  agg
}

extract_root_name <- function(h5_filename) {
  filename <- basename(h5_filename)
  filename <- str_remove(filename, "\\.h5$")
  filename <- str_remove(filename, "\\.analysis$")
  return(filename)
}

scale_rois <- function(roi_data, ref_w, ref_h, cur_w, cur_h) {
  roi_data$x <- roi_data$x * (cur_w / ref_w)
  roi_data$y <- roi_data$y * (cur_h / ref_h)
  return(roi_data)
}

cm_to_px <- function(cm, px_per_cm) {
  if (is.na(px_per_cm) || px_per_cm <= 0) stop("Calibrate px/cm first.")
  cm * px_per_cm
}

rect_points <- function(cx, cy, w_px, h_px, angle_deg = 0, close_poly = TRUE) {
  hw <- w_px/2; hh <- h_px/2
  base <- matrix(c(-hw,-hh,  hw,-hh,  hw,hh,  -hw,hh), ncol=2, byrow=TRUE)
  th <- angle_deg * pi/180
  R <- matrix(c(cos(th), -sin(th), sin(th), cos(th)), 2, 2)
  pts <- t(R %*% t(base))
  pts[,1] <- pts[,1] + cx; pts[,2] <- pts[,2] + cy
  if (close_poly) pts <- rbind(pts, pts[1,])
  data.frame(x=pts[,1], y=pts[,2])
}

circle_points <- function(cx, cy, r_px, n = 64, close_poly = TRUE) {
  t <- seq(0, 2*pi, length.out = n + as.integer(close_poly))
  data.frame(x = cx + r_px*cos(t), y = cy + r_px*sin(t))
}

poly_centroid <- function(x, y) {
  x2 <- c(x, x[1]); y2 <- c(y, y[1])
  cross <- x2[-1]*y2[-length(y2)] - x2[-length(x2)]*y2[-1]
  A <- sum(cross) / 2
  if (is.na(A) || abs(A) < 1e-8) return(c(mean(x, na.rm=TRUE), mean(y, na.rm=TRUE)))
  cx <- sum((x2[-1] + x2[-length(x2)]) * cross) / (6*A)
  cy <- sum((y2[-1] + y2[-length(y2)]) * cross) / (6*A)
  c(cx, cy)
}

move_roi_by <- function(roi_df, name, dx, dy) {
  idx <- roi_df$Name == name
  roi_df$x[idx] <- roi_df$x[idx] + dx
  roi_df$y[idx] <- roi_df$y[idx] + dy
  roi_df
}

# UI 
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
                    /*make the sidebar stick and scroll independently */
                    .sticky-sidebar {
                    position: sticky;
                    top: 10px;
                    max-height: calc(100vh - 20px);
                    overflow-y: auto;
                    }
                    ")),
    tags$script(HTML("
  var keyCounter = 0;
  var shiftPressed = false;
  
  $(document).on('keydown', function(e) {
    if (e.shiftKey) {
      shiftPressed = true;
    }
    
    // Prevent default for arrow keys when not in input
    if (e.target.nodeName !== 'INPUT' && e.target.nodeName !== 'TEXTAREA') {
      keyCounter++;
      Shiny.setInputValue('arrowKey', {
        key: e.which,
        counter: keyCounter,
        shift: shiftPressed
      });
      
      // Keyboard shortcuts
      if (e.ctrlKey || e.metaKey) {
        if (e.which === 83) { // Ctrl+S = Save ROI
          e.preventDefault();
          Shiny.setInputValue('hotkey_save', Math.random());
        }
        if (e.which === 90) { // Ctrl+Z = Undo point
          e.preventDefault();
          Shiny.setInputValue('hotkey_undo', Math.random());
        }
      }
      
      // ESC = Deselect
      if (e.which === 27) {
        Shiny.setInputValue('hotkey_deselect', Math.random());
      }
      
      e.preventDefault();
    }
  });
  
  $(document).on('keyup', function(e) {
    if (!e.shiftKey) {
      shiftPressed = false;
    }
  });
  
  // Send shift status with plot clicks
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'plot_click') {
      Shiny.setInputValue('shift_held', shiftPressed);
    }
  });
  "))
  ),
  
  titlePanel("PACERTrace: ROI-based Behavior Analysis (v1.0.0)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      class = "sticky-sidebar",
      
      # Video Upload Section
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          h4("Video Setup", style = "color: #2c3e50; margin-top: 0;"),
          fileInput("video", "Upload Video File (MP4):", 
                    accept = ".mp4",
                    buttonLabel = "Browse...",
                    placeholder = "No file selected"),
          div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              numericInput("frame_sec", "Extract frame at:", value = 10, min = 0, width = "100px"),
              span("seconds", style = "color: #6c757d;")
          ),
          actionButton("extract", "Extract Frame for ROI Drawing", 
                       class = "btn-primary", style = "width: 100%;")
      ),
      
      # Line calibration
      div(style = "background:#eef5ff; padding: 12px; border-radius: 8px; margin-bottom: 15px;",
          h4("Line Calibration"),
          p("Click two points on the frame to draw a line, then tell me its real length."),
          div(style="display:flex; gap:10px; align-items:center; flex-wrap:wrap;",
              actionButton("calib_start", "Start line"),
              actionButton("calib_cancel", "Cancel", class = "btn-outline-dark"),
              actionButton("calib_reset", "Reset", class = "btn-outline-secondary")
          ),
          div(style="display:flex; gap:10px; align-items:end; margin-top:10px; flex-wrap:wrap;",
              numericInput("calib_length", "Real length:", value = NA, min = 0, width = "140px"),
              selectInput("calib_units", "Units:", c("cm","mm","in"), selected = "cm", width = "100px"),
              actionButton("calib_apply", "Apply")
          ),
          tags$small("Tip: draw along a known edge or chamber arm on the floor plane."),
          hr(),
          tags$div(style="font-family:monospace;",
                   "Pixel length: ", textOutput("calib_pixlen", inline = TRUE),
                   ";  px/cm: ", textOutput("calib_px_per_cm", inline = TRUE)
          )
      ),
      
      # ROI Builder
      div(style = "background-color:#f8f9fa; padding:15px; border-radius:8px; margin-bottom:15px;",
          h4("ROI Builder", style="color:#2c3e50; margin-top:0;"),
          
          fileInput("roi_file", "Load Existing ROI Definitions:",
                    accept = ".csv", buttonLabel = "Browse...",
                    placeholder = "No file selected"),
          
          div(style="background:#eefaf1; padding:12px; border-radius:8px;",
              # 1) Shape selector
              div(style="display:flex; gap:10px; flex-wrap:wrap; align-items:end;",
                  selectInput("shape_type", "Shape:",
                              choices = c("Box (square)"="box", "Rectangle"="rect",
                                          "Circle"="circle", "Polygon (freehand)"="poly"),
                              width="220px"),
                  
                  # 2) Dim controls (shown conditionally)
                  conditionalPanel("input.shape_type == 'box'",
                                   numericInput("shape_w_cm", "Width (cm):", value = 3, min = 0.1, step = 0.1, width="150px")
                  ),
                  conditionalPanel("input.shape_type == 'rect'",
                                   numericInput("shape_w_cm", "Width (cm):",  value = 6, min = 0.1, step = 0.1, width="150px"),
                                   numericInput("shape_h_cm", "Height (cm):", value = 3, min = 0.1, step = 0.1, width="150px")
                  ),
                  conditionalPanel("input.shape_type == 'circle'",
                                   numericInput("shape_d_cm", "Diameter (cm):", value = 6, min = 0.1, step = 0.1, width="170px")
                  ),
                  
                  # 3) Rotation (for box/rect only). If empty, treat as 0.
                  conditionalPanel("['box','rect'].includes(input.shape_type)",
                                   sliderInput("shape_angle", "Angle (°):", min = -180, max = 180, value = 0, step = 1, width = "260px")
                  )
              ),
              
              tags$small("Tip: calibrate first (Line Calibration) so sizes in cm place correctly.", style="display:block; margin-top:6px;"),
              
              # replace the row that had Place Shape / Edit vertices
              div(style="display:flex; gap:10px; margin-top:8px; flex-wrap:wrap;",
                  textInput("shape_label", "ROI Name:", placeholder="e.g., ArmA"),
                  actionButton("mode_shape",  "Place Shape",   class="btn-success"),
                  actionButton("mode_poly",   "Draw Polygon",  class="btn-outline-secondary"),
                  actionButton("mode_select", "Select / Move", class="btn-outline-dark"),
                  actionButton("vertex_next", "Next vertex ▶", class="btn-outline-secondary")
              ),
              tags$small(
                "Modes: ",
                tags$b("Select / Move"), " ← normal clicking; ",
                tags$b("Place Shape"), " ← one-click to place box/rect/circle; ",
                tags$b("Draw Polygon"), " ← clicks add points (finish with Save ROI)."
              )
          ),
          
          # Snapping
          div(style="display:flex; gap:12px; align-items:center; margin-top:10px; flex-wrap:wrap;",
              numericInput("snap_distance", "Snap distance (px):", value = 15, min = 5, max = 50, width="140px"),
              checkboxInput("enable_snapping", "Enable snapping", value = TRUE)
          )
      ),
      
      # Move step (for arrow-key nudging)
      div(style="display:flex; gap:10px; align-items:center; margin-top:10px;",
          numericInput("move_step", "Move step:", value = 5, min = 1, max = 50, width = "90px"),
          span("pixels", style="color:#6c757d; font-size:12px;")
      ),
      
      # Align selected ROI to target ROI (centroid-to-centroid)
      div(style="display:flex; gap:10px; align-items:end; margin-top:10px; flex-wrap:wrap;",
          selectInput("align_target", "Align to ROI:", choices = NULL, width = "160px"),
          actionButton("align_center", "Center on target", class = "btn-outline-primary")
      ),
      
      
      # Save / Delete for selected ROI
      div(style="display:flex; gap:8px; margin-top:10px;",
          actionButton("save_roi",   "Save Current Poly", class="btn-success"),
          actionButton("delete_roi", "Delete Selected",    class="btn-danger")
      ),
      
      downloadButton("download_rois", "Download ROI Definitions",
                     class = "btn-info", style = "width: 100%; margin-top: 10px;"),
     
       # ROI debug button
      actionButton("debug_roi", "Debug ROI Data", 
                   class = "btn-outline-info", 
                   style = "width: 100%; margin-top: 10px;"),
      
      # Analysis Section  
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          h4("SLEAP Analysis", style = "color: #2c3e50; margin-top: 0;"),
          fileInput("h5file", "Upload SLEAP .h5 File:", 
                    accept = ".h5",
                    buttonLabel = "Browse...",
                    placeholder = "No file selected"),
          actionButton("inspect_h5", "🔍 Inspect H5 Structure", 
                       class = "btn-outline-secondary", style = "width: 100%; margin-bottom: 10px;"),
          
          div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              numericInput("bin_size", "Time bin size:", value = 10, min = 1, width = "100px"),
              span("seconds", style = "color: #6c757d;")
          ),
          
          div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px;",
              numericInput("interp_gap", "Interpolate gaps ≤ (frames):", value = 3, min = 0, max = 120, width = "200px")
          ),
          
          div(style = "display: flex; gap: 5px; margin-bottom: 10px;",
              actionButton("analyze", "▶️ Run Analysis", class = "btn-warning", style = "flex: 2;"),
              downloadButton("downloadResults", "⬇️ CSV", class = "btn-success", style = "flex: 1;")
          ),
          
          # Frame-by-frame export section
          div(style = "background-color: #e9ecef; padding: 10px; border-radius: 5px; margin-top: 15px;",
              h6("Frame-by-Frame Export", style = "color: #495057; margin-top: 0;"),
              tags$small("Export detailed coordinates and ROI occupancy for every frame", style = "color: #6c757d;"),
              br(), br(),
              downloadButton("downloadFrameData", "⬇️ Download Frame Data CSV", 
                             class = "btn-info", style = "width: 100%;")
          )
      ),
      
      # Debug Section (Collapsible)
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
          h5("🐛 Debug Info", style = "color: #6c757d; margin-top: 0; cursor: pointer;", 
             onclick = "toggleDebug()"),
          div(id = "debug-content", style = "display: none;",
              verbatimTextOutput("debug_console")
          )
      ),
      
      # Add JavaScript for collapsible debug
      tags$script(HTML("
      function toggleDebug() {
        var content = document.getElementById('debug-content');
        if (content.style.display === 'none') {
          content.style.display = 'block';
        } else {
          content.style.display = 'none';
        }
      }
    "))
    ),
    
    mainPanel(
      width = 8,
      
      # Main plot area
      div(style = "background-color: white; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px; margin-bottom: 20px;",
          h4("Frame Viewer & ROI Editor", style = "color: #2c3e50; margin-top: 0;"),
          plotOutput("frameplot", click = "plot_click", height = "500px"),
          div(id = "roi-status", style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
              textOutput("roi_status_text")
          )
      ),
      
      # Results section
      div(style = "background-color: white; border: 1px solid #dee2e6; border-radius: 8px; padding: 15px;",
          h4("Analysis Results", style = "color: #2c3e50; margin-top: 0;"),
          div(id = "results-content",
              # H5 Inspection Results
              conditionalPanel(
                condition = "output.h5_inspection_available",
                div(style = "background-color: #f8f9fa; border-radius: 5px; padding: 15px; margin-bottom: 15px;",
                    h5("🔍 H5 File Inspection Results", style = "color: #495057; margin-top: 0;"),
                    htmlOutput("h5_inspection_output")
                )
              ),
              # Visualization Controls and Plots
              conditionalPanel(
                condition = "output.analysis_available",
                div(style = "background-color: #f8f9fa; border-radius: 5px; padding: 15px;",
                    h5("📈 Data Visualization", style = "color: #495057; margin-top: 0;"),
                    #add node picker to controls row
                    
                    div(style = "flex: 1; min-width: 180px;",
                        selectInput("plot_node", "Body Part:", choices = character(0), width = "100%")
                    ),
                    
                    
                    # Enhanced Controls row with temporal heatmap options
                    div(style = "display: flex; gap: 15px; align-items: end; margin-bottom: 20px; flex-wrap: wrap;",
                        div(style = "flex: 1; min-width: 200px;",
                            pickerInput("traj_roi", 
                                        "Overlay ROIs (dashed):", 
                                        choices = character(0), 
                                        multiple = TRUE,
                                        options = list(
                                          `actions-box` = TRUE,
                                          `selected-text-format` = "count > 2",
                                          `count-selected-text` = "{0} ROIs selected"
                                        ),
                                        width = "100%")
                        ),
                        div(style = "flex: 1; min-width: 150px;",
                            selectInput("plot_type", "Plot Type:", 
                                        choices = list(
                                          "Position Heatmap" = "heatmap",
                                          "Temporal Heatmap" = "temporal_heatmap",
                                          "Outside ROI Heatmap" = "outside_roi_heatmap",
                                          "Detection Raster" = "raster", 
                                          "Missing Data %" = "missing",
                                          "ROI Time Spent" = "roi_time",
                                          "Cumulative Distance" = "cumdist",
                                          "Speed (per-second)" = "speed",
                                          "Distance per bin" = "dist_bin",
                                          "Trajectory(path)" = "traj"
                                        ), 
                                        selected = "traj", width = "100%")
                        ),
                        # Time bin selector for temporal heatmap and raster
                        conditionalPanel(
                          condition = "input.plot_type == 'raster' || input.plot_type == 'temporal_heatmap'",
                          div(style = "flex: 1; min-width: 120px;",
                              selectInput("plot_time_bin", "Time Bin (sec):", 
                                          choices = list("10" = 10, "30" = 30, "60" = 60, "120" = 120, "300" = 300), 
                                          selected = 60, width = "100%")
                          )
                        )
                    ),
                    
                    # Additional controls for temporal heatmap
                    conditionalPanel(
                      condition = "input.plot_type == 'temporal_heatmap'",
                      div(style = "background-color: #e9ecef; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                          h6("Temporal Heatmap Settings", style = "margin-top: 0;"),
                          div(style = "display: flex; gap: 10px; align-items: center; flex-wrap: wrap;",
                              div(style = "flex: 1; min-width: 120px;",
                                  numericInput("heatmap_bins", "Spatial Bins:", value = 25, min = 10, max = 100, width = "100%")
                              ),
                              div(style = "flex: 1; min-width: 120px;",
                                  selectInput("heatmap_method", "Method:", 
                                              choices = list("Density" = "density", "Count" = "count"), 
                                              selected = "density", width = "100%")
                              ),
                              div(style = "flex: 1; min-width: 120px;",
                                  checkboxInput("normalize_axes", "Normalize to image", value = TRUE)
                              )
                          )
                      )
                    ),
                    
                    # --- Trajectory (per-bin) controls ---
                    # --- Trajectory (per-bin) controls ---
                    conditionalPanel(
                      condition = "input.plot_type == 'traj'",
                      div(style = "background-color: #e9ecef; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                          h6("Trajectory Options", style = "margin-top: 0;"),
                          div(style = "display:flex; gap:10px; align-items:end; flex-wrap:wrap;",
                              # bin size for trajectory plots
                              div(style = "flex: 1; min-width: 140px;",
                                  selectInput("traj_bin_sec", "Time Bin (sec):",
                                              choices = list("10"=10, "30"=30, "60"=60, "120"=120, "300"=300),
                                              selected = 60, width = "100%")
                              ),
                              # facet vs single
                              div(style = "flex: 1; min-width: 220px;",
                                  radioButtons("traj_bin_mode", "Show:",
                                               choices = c("Facet all bins" = "facet", "Single bin" = "single"),
                                               selected = "facet", inline = TRUE)
                              ),
                              # show "Which bin" ONLY when single is chosen
                              conditionalPanel("input.traj_bin_mode == 'single'",
                                               div(style = "flex: 1; min-width: 200px;",
                                                   selectInput("traj_bin_pick", "Which bin:", choices = character(0), width = "100%")
                                               )
                              ),
                              div(style = "flex: 1; min-width: 180px;",
                                  checkboxInput("traj_clip_to_roi", "Clip path to ROI", value = FALSE)
                              ),
                              # IMPORTANT: remove the duplicate ROI control here (keep the top-level pickerInput 'traj_roi')
                              # div(style = "flex: 1; min-width: 200px;",
                              #     selectInput("traj_roi", "Overlay ROI (dashed):", choices = character(0), width = "100%")
                              # ),
                              div(style = "flex: 1; min-width: 180px;",
                                  checkboxInput("traj_use_image_limits", "Normalize to image", value = TRUE)
                              )
                          )
                      )
                    )
                    ,
                    
                    
                    # Action buttons row
                    div(style = "display: flex; gap: 10px; margin-bottom: 20px; align-items: center;",
                        actionButton("update_plot", "🔄 Update Plot", 
                                     class = "btn-primary"),
                        # Export plot buttons
                        downloadButton("export_plot_pdf", "Export PDF", 
                                       class = "btn-secondary"),
                        downloadButton("export_plot", "Export PNG", class = "btn-secondary")
                    ),
                    
                    # Plot output
                    div(style = "background-color: white; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px;",
                        plotOutput("analysis_plot", height = "400px")
                    )
                    
                )
              ),
              # Analysis Results Table
              div(style = "margin-bottom: 20px;",
                  h5("Results Summary", style = "color: #495057;"),
                  tableOutput("results_preview")
              )
          )
      )
    ),
    
    position = "left"
  )
)

# Server
server <- function(input, output, session) {
  frame_path <- reactiveVal()
  roi_list <- reactiveVal(data.frame(Name = character(), x = numeric(), y = numeric(), stringsAsFactors = FALSE))
  ref_dims <- reactiveVal(c(NA, NA))
  analysis_results <- reactiveVal(NULL)
  polygon_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  selected_roi <- reactiveVal(NULL)  # Track which ROI is selected for moving
  selected_vertex <- reactiveVal(NULL)
  move_step <- reactiveVal(5)  # How many pixels to move with each arrow key press
  h5_inspection_results <- reactiveVal(NULL)  # Store H5 inspection results
  node_coordinates <- reactiveVal(NULL)  # Store all node coordinates for plotting
  frame_roi_data <- reactiveVal(NULL)  # Store frame-by-frame ROI data
  current_plot <- reactiveVal(NULL)  # Store current plot for export
  video_fps <- reactiveVal(NULL)  # Store video FPS
  roi_coverage_debug <- reactiveVal(NULL)  # Store ROI coverage debug info
  image_dimensions <- reactiveVal(c(NA, NA))  # Store image dimensions for normalization
  outside_roi_data <- reactiveVal(NULL)  # Store outside ROI analysis
  calib_active <- reactiveVal(FALSE)
  calib_pts <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  calib_pixlen <- reactiveVal(NA_real_)
  calib_px_per_cm <- reactiveVal(NA_real_)
  calib_length_cm_applied <- reactiveVal(NA_real_)
  calib_units_applied     <- reactiveVal(NA_character_)
  edit_mode <- reactiveVal("select")
  
  edit_mode <- reactiveVal("select")
  
  # ===== KEYBOARD SHORTCUTS =====
  
  # Ctrl+S / Cmd+S: Quick save current polygon
  observeEvent(input$hotkey_save, {
    current_points <- polygon_points()
    label <- input$shape_label
    
    if (nrow(current_points) >= 3 && !is.null(label) && nzchar(trimws(label))) {
      shinyjs::click("save_roi")
      showNotification("⌨️ Ctrl+S: ROI saved!", type = "message", duration = 1.5)
    } else if (nrow(current_points) < 3) {
      showNotification("⌨️ Ctrl+S: Need at least 3 points to save", 
                       type = "warning", duration = 2)
    } else {
      showNotification("⌨️ Ctrl+S: Please enter an ROI name first", 
                       type = "warning", duration = 2)
    }
  })
  
  # Ctrl+Z / Cmd+Z: Undo last point
  observeEvent(input$hotkey_undo, {
    current_points <- polygon_points()
    if (nrow(current_points) > 0) {
      updated_points <- current_points[-nrow(current_points), , drop = FALSE]
      polygon_points(updated_points)
      showNotification(
        paste("⌨️ Ctrl+Z: Removed last point -", nrow(updated_points), "remaining"), 
        type = "message", 
        duration = 1
      )
    } else {
      showNotification("⌨️ Ctrl+Z: No points to undo", type = "default", duration = 1)
    }
  })
  
  # ESC: Deselect current ROI
  observeEvent(input$hotkey_deselect, {
    if (!is.null(selected_roi()) || !is.null(selected_vertex())) {
      selected_roi(NULL)
      selected_vertex(NULL)
      showNotification("⌨️ ESC: Deselected", type = "message", duration = 1)
    }
  })
  
  # DELETE or Ctrl+D: Delete selected ROI
  observeEvent(input$hotkey_delete, {
    if (!is.null(selected_roi())) {
      shinyjs::click("delete_roi")
    } else {
      showNotification("⌨️ DELETE: No ROI selected", type = "default", duration = 1.5)
    }
  })
  
  # ===== END KEYBOARD SHORTCUTS =====
  
  # Keep Trajectory overlay ROI list updated
  observe({
    rd <- roi_list()
    if (nrow(rd) > 0) {
      roi_names <- sort(unique(rd$Name))
      updatePickerInput(session, "traj_roi", choices = roi_names)
    } else {
      updatePickerInput(session, "traj_roi", choices = character(0))
    }
  })
  

  # Build choices for the Single-bin picker whenever node or bin size changes
  observe({
    tryCatch({
      # Validate inputs exist first
      if (is.null(node_coordinates()) || nrow(node_coordinates()) == 0) {
        return(NULL)
      }
      
      if (is.null(input$plot_node) || length(input$plot_node) == 0 || input$plot_node == "") {
        return(NULL)
      }
      
      if (is.null(input$traj_bin_sec) || length(input$traj_bin_sec) == 0) {
        return(NULL)
      }
      
      # choose bin size for trajectory
      bs <- suppressWarnings(as.numeric(input$traj_bin_sec))
      
      # Check if bs has length zero or is invalid
      if (length(bs) == 0) {
        bs <- 60
      } else if (is.na(bs) || bs <= 0) {
        bs <- 60
      }
      
      nc <- node_coordinates()
      sel <- nc[nc$Node == input$plot_node & nc$Valid, c("Time_sec"), drop = FALSE]
      
      if (nrow(sel) == 0) {
        updateSelectInput(session, "traj_bin_pick", choices = character(0))
        return(NULL)
      }
      
      max_t <- suppressWarnings(max(sel$Time_sec, na.rm = TRUE))
      
      # Check length FIRST
      if (length(max_t) == 0) {
        updateSelectInput(session, "traj_bin_pick", choices = character(0))
        return(NULL)
      }
      
      # Now check for invalid values (split into separate checks)
      if (is.na(max_t)) {
        updateSelectInput(session, "traj_bin_pick", choices = character(0))
        return(NULL)
      }
      
      if (is.infinite(max_t)) {
        updateSelectInput(session, "traj_bin_pick", choices = character(0))
        return(NULL)
      }
      
      brks  <- seq(0, max_t + bs, by = bs)
      labs  <- paste0(sprintf("%.0f", head(brks, -1)), "–", sprintf("%.0f", tail(brks, -1)), " s")
      
      updateSelectInput(session, "traj_bin_pick", choices = labs, selected = labs[1])
      
    }, error = function(e) {
      # Silent error handling - just don't update the picker
      cat("Note: Trajectory bin picker update skipped due to:", e$message, "\n")
    })
  })
  
  # One source of truth for scale from line calibration
  px_per_cm <- reactive({
    val <- calib_px_per_cm()
    if (is.null(val) || is.na(val) || val <= 0) NA_real_ else val
  })
  
  observe({
    if (is.na(px_per_cm()) || px_per_cm() <= 0) {
      shinyjs::disable("mode_shape")
    } else {
      shinyjs::enable("mode_shape")
    }
  })
  
  # Update move step when input changes
  observeEvent(input$move_step, {
    move_step(input$move_step)
  })
  
  # Keep the Align dropdown in sync with current ROIs
  observe({
    rd <- roi_list()
    if (nrow(rd) > 0) {
      updateSelectInput(session, "align_target", choices = sort(unique(rd$Name)))
    } else {
      updateSelectInput(session, "align_target", choices = character(0))
    }
  })
  

  observeEvent(input$extract, {
    req(input$video)
    path <- input$video$datapath
    frame_path(extract_frame(path, input$frame_sec))
    img <- image_read(frame_path())
    dims <- image_info(img)
    ref_dims(c(dims$width, dims$height))
    image_dimensions(c(dims$width, dims$height))  # Store for normalization
    
    # Store FPS for later use
    fps <- extract_fps(path)
    video_fps(fps)
    
    shinyjs::enable("analyze")
  })
  
  # NEW: H5 file inspection with UI output
  observeEvent(input$inspect_h5, {
    req(input$h5file)
    
    inspection_result <- tryCatch({
      h5 <- H5File$new(input$h5file$datapath, mode = "r")
      
      result <- list(
        status = "success",
        filename = basename(input$h5file$name),
        datasets = names(h5),
        issues = character(0),
        details = list()
      )
      
      # Check for required datasets
      if (!"tracks" %in% names(h5)) {
        result$status <- "error"
        result$issues <- c(result$issues, "❌ No 'tracks' dataset found")
      } else {
        tracks <- h5[["tracks"]]$read()
        track_dims <- length(dim(tracks))
        track_shape <- dim(tracks)
        
        result$details$tracks <- list(
          shape = track_shape,
          dimensions = track_dims,
          n_frames = track_shape[1]
        )
        
        if (track_dims == 4) {
          result$details$tracks$structure <- "4D: [frames, nodes, coordinates, instances]"
          result$details$tracks$n_nodes <- track_shape[2]
          result$details$tracks$n_coords <- track_shape[3]
          result$details$tracks$n_instances <- track_shape[4]
          result$issues <- c(result$issues, "✅ 4D track structure detected (with instances)")
        } else if (track_dims == 3) {
          result$details$tracks$structure <- "3D: [frames, nodes, coordinates]"
          result$details$tracks$n_nodes <- track_shape[2]
          result$details$tracks$n_coords <- track_shape[3]
          result$issues <- c(result$issues, "✅ 3D track structure detected (no instances)")
        } else {
          result$status <- "warning"
          result$issues <- c(result$issues, paste("⚠️ Unexpected track structure with", track_dims, "dimensions"))
        }
        
        # Check for NaN/infinite values in first few frames
        if (track_dims == 4) {
          sample_data <- tracks[1:min(10, track_shape[1]), , , 1]
        } else {
          sample_data <- tracks[1:min(10, track_shape[1]), , ]
        }
        
        nan_count <- sum(is.na(sample_data))
        inf_count <- sum(is.infinite(sample_data))
        total_sample <- length(sample_data)
        
        if (nan_count > 0 || inf_count > 0) {
          result$issues <- c(result$issues, paste("⚠️", nan_count, "NaN and", inf_count, "infinite values found in sample"))
        } else {
          result$issues <- c(result$issues, "✅ No NaN/infinite values in sample data")
        }
      }
      
      # Check node names
      if (!"node_names" %in% names(h5)) {
        result$status <- "error"
        result$issues <- c(result$issues, "❌ No 'node_names' dataset found")
      } else {
        node_names_raw <- h5[["node_names"]]$read()
        
        # Process node names
        if (is.list(node_names_raw)) {
          processed_names <- sapply(node_names_raw, function(x) {
            if (is.raw(x)) {
              rawToChar(x[x != 0])
            } else {
              as.character(x)
            }
          })
        } else {
          processed_names <- sapply(node_names_raw, function(x) gsub("\\x00", "", x))
        }
        processed_names <- trimws(processed_names)
        
        result$details$node_names <- processed_names
        result$issues <- c(result$issues, paste("✅", length(processed_names), "body parts found:", paste(processed_names, collapse = ", ")))
        
        # Check if node count matches tracks
        if ("tracks" %in% names(h5) && length(processed_names) == result$details$tracks$n_nodes) {
          result$issues <- c(result$issues, "✅ Node count matches track dimensions")
        } else if ("tracks" %in% names(h5)) {
          result$status <- "warning"
          result$issues <- c(result$issues, paste("⚠️ Node count mismatch: found", length(processed_names), "names but", result$details$tracks$n_nodes, "nodes in tracks"))
        }
      }
      
      h5$close_all()
      
      # Overall assessment
      if (result$status == "success") {
        if ("tracks" %in% result$datasets && "node_names" %in% result$datasets) {
          result$overall <- "✅ File appears compatible with PACERTrace analysis"
        } else {
          result$overall <- "⚠️ File missing required datasets"
          result$status <- "warning"
        }
      } else if (result$status == "warning") {
        result$overall <- "⚠️ File has some issues but may still work"
      } else {
        result$overall <- "❌ File has critical issues and may not work"
      }
      
      result
      
    }, error = function(e) {
      list(
        status = "error",
        filename = basename(input$h5file$name),
        overall = paste("❌ Error reading file:", e$message),
        issues = paste("Error:", e$message),
        details = list()
      )
    })
    
    h5_inspection_results(inspection_result)
    
    # Show notification based on status
    if (inspection_result$status == "success") {
      showNotification("H5 file inspection completed - File looks good!", 
                       type = "message", duration = 3)
    } else if (inspection_result$status == "warning") {
      showNotification("H5 file inspection completed - Some issues found", 
                       type = "warning", duration = 5)
    } else {
      showNotification("H5 file inspection completed - Critical issues found", 
                       type = "error", duration = 5)
    }
  })
  
  observeEvent(input$roi_file, {
    req(frame_path())
    
    result <- tryCatch({
      roi_data <- read.csv(input$roi_file$datapath, stringsAsFactors = FALSE)
      
      # Validate required columns
      required_cols <- c("Name", "x", "y")
      missing_cols <- setdiff(required_cols, names(roi_data))
      
      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
      
      if (nrow(roi_data) == 0) {
        stop("ROI file is empty")
      }
      
      # RESTORE CALIBRATION if it exists in the file
      if ("Calib_PxPerCm" %in% names(roi_data)) {
        calib_val <- unique(roi_data$Calib_PxPerCm)[1]
        if (!is.na(calib_val) && calib_val > 0) {
          calib_px_per_cm(calib_val)
          
          # Restore other calibration metadata if available
          if ("Calib_LineLength_px" %in% names(roi_data)) {
            calib_pixlen(unique(roi_data$Calib_LineLength_px)[1])
          }
          if ("Calib_RealLength_cm" %in% names(roi_data)) {
            calib_length_cm_applied(unique(roi_data$Calib_RealLength_cm)[1])
          }
          if ("Calib_Units" %in% names(roi_data)) {
            calib_units_applied(unique(roi_data$Calib_Units)[1])
          }
          
          # Restore calibration points if available
          if (all(c("Calib_Point1_X", "Calib_Point1_Y", "Calib_Point2_X", "Calib_Point2_Y") %in% names(roi_data))) {
            pt1_x <- unique(roi_data$Calib_Point1_X)[1]
            pt1_y <- unique(roi_data$Calib_Point1_Y)[1]
            pt2_x <- unique(roi_data$Calib_Point2_X)[1]
            pt2_y <- unique(roi_data$Calib_Point2_Y)[1]
            
            if (!any(is.na(c(pt1_x, pt1_y, pt2_x, pt2_y)))) {
              calib_pts(data.frame(x = c(pt1_x, pt2_x), y = c(pt1_y, pt2_y)))
            }
          }
          
          cat("✓ Calibration restored: px/cm =", calib_val, "\n")
        }
      }
      
      # Get current frame dimensions
      dims <- image_info(image_read(frame_path()))
      
      # Scale ROIs if dimension info is available
      scaled <- FALSE
      if ("FrameWidth" %in% names(roi_data) && "FrameHeight" %in% names(roi_data)) {
        ref_w <- unique(roi_data$FrameWidth)[1]
        ref_h <- unique(roi_data$FrameHeight)[1]
        
        if (!is.na(ref_w) && !is.na(ref_h) && (ref_w != dims$width || ref_h != dims$height)) {
          cat("Scaling ROIs from", ref_w, "x", ref_h, "to", dims$width, "x", dims$height, "\n")
          roi_data <- scale_rois(roi_data, ref_w, ref_h, dims$width, dims$height)
          scaled <- TRUE
          
          # Also scale calibration points
          pts <- calib_pts()
          if (nrow(pts) == 2) {
            pts$x <- pts$x * (dims$width / ref_w)
            pts$y <- pts$y * (dims$height / ref_h)
            calib_pts(pts)
          }
        }
      }
      
      # Keep only essential columns for ROI display
      essential_cols <- c("Name", "x", "y")
      optional_cols <- c("FrameWidth", "FrameHeight", "VideoFile", "VideoPath", 
                         "FrameTime_sec", "CreatedDate", "CreatedTime")
      cols_to_keep <- c(essential_cols, intersect(optional_cols, names(roi_data)))
      roi_data <- roi_data[, cols_to_keep, drop = FALSE]
      
      # Return success
      list(
        success = TRUE, 
        data = roi_data,
        n_rois = length(unique(roi_data$Name)),
        n_points = nrow(roi_data),
        scaled = scaled,
        calibration_restored = !is.na(calib_px_per_cm()) && calib_px_per_cm() > 0,
        x_range = range(roi_data$x, na.rm = TRUE),
        y_range = range(roi_data$y, na.rm = TRUE)
      )
      
    }, error = function(e) {
      list(success = FALSE, error = as.character(e$message))
    })
    
    # Handle result
    if (result$success) {
      roi_list(result$data)
      
      msg <- paste0(
        "✓ Loaded ", result$n_rois, " ROI(s) with ", result$n_points, " points\n",
        if (result$scaled) "📐 Scaled to match current frame\n" else "",
        if (result$calibration_restored) paste0("📏 Calibration restored: ", round(calib_px_per_cm(), 3), " px/cm\n") else "",
        "X range: ", round(result$x_range[1]), " to ", round(result$x_range[2]), "\n",
        "Y range: ", round(result$y_range[1]), " to ", round(result$y_range[2])
      )
      
      showNotification(msg, type = "message", duration = 5)
      
      cat("\n=== ROI File Loaded Successfully ===\n")
      cat("File:", input$roi_file$name, "\n")
      cat("Number of ROIs:", result$n_rois, "\n")
      cat("Calibration restored:", result$calibration_restored, "\n")
      cat("====================================\n\n")
      
    } else {
      showNotification(
        paste("✗ Error loading ROI file:\n", result$error),
        type = "error",
        duration = 10
      )
      cat("\n=== ROI File Loading Error ===\n")
      cat("Error:", result$error, "\n")
      cat("==============================\n\n")
    }
  })
  
  observeEvent(input$plot_click, {
    req(frame_path())
    
    # ---- Calibration clicks take priority ----
    if (isTRUE(calib_active())) {
      pts <- calib_pts()
      pts <- rbind(pts, data.frame(x = input$plot_click$x, y = input$plot_click$y))
      calib_pts(pts)
      if (nrow(pts) == 2) {
        dpx <- sqrt((pts$x[2]-pts$x[1])^2 + (pts$y[2]-pts$y[1])^2)
        calib_pixlen(dpx)
        calib_active(FALSE)
        showNotification(
          paste("Line length:", round(dpx,2), "px. Enter real length and click Apply."),
          type = "message"
        )
      } else {
        showNotification("Point 1 recorded. Click the second point.", type="message")
      }
      return(NULL)
    }
    
    # Common inputs
    cx <- input$plot_click$x
    cy <- input$plot_click$y
    snap_enabled <- isTRUE(input$enable_snapping)
    snap_dist    <- if (is.null(input$snap_distance)) 15 else input$snap_distance
    shift_held   <- isTRUE(input$shift_held)
    
    # ---------- MODE: POLYGON DRAW ----------
    if (identical(edit_mode(), "poly")) {
      # Add a vertex (with optional snapping)
      if (snap_enabled && nrow(roi_list()) > 0) {
        near <- find_nearby_edges(cx, cy, roi_list(), snap_dist)
        if (!is.null(near) && (shift_held || near$distance <= snap_dist/2)) {
          cx <- near$x; cy <- near$y
        }
      }
      pts <- polygon_points()
      if (nrow(pts) >= 1) {
        d0 <- sqrt((cx - pts$x[1])^2 + (cy - pts$y[1])^2)
        if (d0 <= max(8, snap_dist/2)) { cx <- pts$x[1]; cy <- pts$y[1] }
      }
      polygon_points(rbind(pts, data.frame(x = cx, y = cy)))
      
      # Optional shared-edge suggestion
      if (nrow(polygon_points()) >= 2 && nrow(roi_list()) > 0) {
        suggestion <- suggest_shared_edge(polygon_points(), roi_list())
        if (!is.null(suggestion)) {
          showNotification(
            paste("Consider sharing edge with", suggestion$roi, "for seamless coverage"),
            type = "default", duration = 3
          )
        }
      }
      return(NULL)
    }
    
    # ---------- MODE: SHAPE PLACE (box/rect/circle) ----------
    if (identical(edit_mode(), "shape") && input$shape_type %in% c("box", "rect", "circle")) {
      # Require calibration (we’re converting cm -> px)
      ppcm <- px_per_cm()
      if (is.na(ppcm) || ppcm <= 0) {
        showNotification("Calibrate first (📏 Line Calibration).", type = "error")
        return(NULL)
      }
      
      # Label
      label <- if (nzchar(trimws(input$shape_label))) trimws(input$shape_label)
      else paste0(toupper(input$shape_type), "_", sample(1000:9999, 1))
      
      ang <- if (is.null(input$shape_angle) || is.na(input$shape_angle)) 0 else input$shape_angle
      
      shp <- switch(input$shape_type,
                    "box" = {
                      w_px <- cm_to_px(req(input$shape_w_cm), ppcm)
                      rect_points(cx, cy, w_px, w_px, angle_deg = ang)
                    },
                    "rect" = {
                      w_px <- cm_to_px(req(input$shape_w_cm), ppcm)
                      h_px <- cm_to_px(req(input$shape_h_cm), ppcm)
                      rect_points(cx, cy, w_px, h_px, angle_deg = ang)
                    },
                    "circle" = {
                      r_px <- cm_to_px(req(input$shape_d_cm) / 2, ppcm)
                      circle_points(cx, cy, r_px, n = 72)
                    }
      )
      
      # Append to ROI table
      cur <- roi_list()
      add <- data.frame(Name = label, x = shp$x, y = shp$y, stringsAsFactors = FALSE)
      
      # Match columns between existing and new ROI data
      if (nrow(cur) > 0) {
        # Get all unique column names
        all_cols <- union(names(cur), names(add))
        
        # Add missing columns to 'add' with appropriate default values
        for (col in setdiff(all_cols, names(add))) {
          if (col %in% c("FrameWidth", "FrameHeight", "FrameTime_sec")) {
            add[[col]] <- NA_real_
          } else if (col %in% c("VideoFile", "VideoPath")) {
            add[[col]] <- NA_character_
          } else if (col == "CreatedDate") {
            add[[col]] <- as.Date(NA)
          } else if (col == "CreatedTime") {
            add[[col]] <- NA_character_
          } else {
            add[[col]] <- NA
          }
        }
        
        # Add missing columns to 'cur' (shouldn't be needed, but just in case)
        for (col in setdiff(all_cols, names(cur))) {
          if (col %in% c("x", "y")) {
            cur[[col]] <- NA_real_
          } else if (col == "Name") {
            cur[[col]] <- NA_character_
          } else {
            cur[[col]] <- NA
          }
        }
        
        # Reorder columns to match
        add <- add[names(cur)]
        
        # Now rbind will work
        roi_list(rbind(cur, add))
      } else {
        # First ROI - just use add as-is
        roi_list(add)
      }
      
      selected_roi(label)
      selected_vertex(NULL)
      showNotification(paste("Placed", input$shape_type, "ROI:", label), type = "message")
      
      return(NULL)
    }
    
    # ---------- MODE: SELECT / MOVE (default) ----------
    # Try selecting an ROI (and maybe a nearby vertex)
    roi_data <- roi_list()
    clicked_roi <- NULL
    
    if (nrow(roi_data) > 0) {
      roi_names <- unique(roi_data$Name)
      for (roi_name in roi_names) {
        roi_points <- roi_data[roi_data$Name == roi_name, ]
        if (nrow(roi_points) >= 3) {
          inside <- sp::point.in.polygon(cx, cy, roi_points$x, roi_points$y)
          if (inside > 0) { clicked_roi <- roi_name; break }
        }
      }
    }
    
    if (!is.null(clicked_roi)) {
      selected_roi(clicked_roi)
      
      # Try to select a vertex close to the click
      sel <- roi_data[roi_data$Name == clicked_roi, ]
      v_tol <- 12
      if (nrow(sel) >= 1) {
        d <- sqrt((sel$x - cx)^2 + (sel$y - cy)^2)
        k <- which.min(d)
        if (length(k) && d[k] <= v_tol) {
          selected_vertex(list(roi = clicked_roi, idx = k))
          showNotification(paste("Selected ROI:", clicked_roi, "— vertex", k, "ready (use arrow keys)"),
                           type = "message", duration = 2)
        } else {
          selected_vertex(NULL)
          showNotification(paste("Selected ROI:", clicked_roi, "— use arrow keys to move"),
                           type = "message", duration = 2)
        }
      } else {
        selected_vertex(NULL)
        showNotification(paste("Selected ROI:", clicked_roi), type = "message", duration = 2)
      }
      return(NULL)
    } else {
      # Clicked outside any ROI → deselect
      selected_roi(NULL); selected_vertex(NULL)
      showNotification("ROI deselected", type = "message", duration = 1.5)
      return(NULL)
    }
  })
  
  observeEvent(input$calib_start, {
    calib_active(TRUE)
    calib_pts(data.frame(x = numeric(), y = numeric()))
    calib_pixlen(NA_real_)
    showNotification("Calibration: click TWO points on the frame to draw the line.", type = "message")
  })
  
  observeEvent(input$calib_reset, {
    calib_active(FALSE)
    calib_pts(data.frame(x = numeric(), y = numeric()))
    calib_pixlen(NA_real_)
    calib_px_per_cm(NA_real_)
  })
  
  observeEvent(input$calib_cancel, {
    # stop listening for clicks, keep any drawn line so the user can still Apply
    calib_active(FALSE)
    showNotification("Calibration cancelled (line kept).", type = "warning")
  })
  
  # When user enters the real length and clicks Apply, compute px/cm
  observeEvent(input$calib_apply, {
    dpx <- calib_pixlen()
    if (is.na(dpx) || dpx <= 0) { showNotification("Draw the line first (two clicks).", type="warning"); return() }
    d <- suppressWarnings(as.numeric(input$calib_length))
    if (is.null(d) || is.na(d) || d <= 0) { showNotification("Enter a positive real length.", type="warning"); return() }
    
    units <- if (is.null(input$calib_units)) "cm" else input$calib_units
    d_cm <- switch(units, "cm" = d, "mm" = d/10, "in" = d*2.54, d)
    
    px_per_cm_val <- dpx / d_cm
    calib_px_per_cm(px_per_cm_val)
    
    # store what was actually applied
    calib_length_cm_applied(d_cm)
    calib_units_applied(units)
    
    showNotification(paste("px/cm set to", round(px_per_cm_val, 3)), type="message")
  })

  
  # Little outputs for the blue card
  output$calib_pixlen <- renderText({
    if (is.na(calib_pixlen())) "—" else round(calib_pixlen(), 2)
  })
  output$calib_px_per_cm <- renderText({
    if (is.na(calib_px_per_cm())) "—" else round(calib_px_per_cm(), 3)
  })
  
  # Build the dimension inputs dynamically based on shape type
  output$shape_dims_ui <- renderUI({
    st <- input$shape_type
    if (is.null(st)) st <- "box"
    switch(st,
           "box" = tagList(
             numericInput("shape_w_cm", "Width (cm):", value = 10, min = 0.1, step = 0.1, width = "160px")
           ),
           "rect" = tagList(
             numericInput("shape_w_cm", "Width (cm):",  value = 12, min = 0.1, step = 0.1, width = "160px"),
             numericInput("shape_h_cm", "Height (cm):", value = 8,  min = 0.1, step = 0.1, width = "160px")
           ),
           "circle" = tagList(
             numericInput("shape_d_cm", "Diameter (cm):", value = 10, min = 0.1, step = 0.1, width = "180px")
           ),
           # poly (freehand) — no dimension inputs
           tagList()
    )
  })
  
  
  # FIXED: Handle arrow key movements with unique counter support
  observeEvent(input$arrowKey, {
    req(selected_roi())
    req(nrow(roi_list()) > 0)
    
    roi_data <- roi_list()
    selected_name <- selected_roi()
    
    # Arrow key info from JS
    key_code <- isolate(input$arrowKey$key)        # 37=←, 38=↑, 39=→, 40=↓
    fast     <- isTRUE(isolate(input$arrowKey$shift))
    step     <- move_step() * if (fast) 5 else 1
    
    # Map key -> delta (note: y increases downward in image coords)
    dx <- switch(as.character(key_code),
                 "37" = -step,  # left
                 "39" =  step,  # right
                 "38" =  0,     # up  (dy below)
                 "40" =  0,
                 0)
    dy <- switch(as.character(key_code),
                 "37" =  0,
                 "39" =  0,
                 "38" = -step,  # up    (smaller y)
                 "40" =  step,  # down  (larger y)
                 0)
    
    # Ignore non-arrow keys
    if (dx == 0 && dy == 0) return(NULL)
    
    roi_idx <- which(roi_data$Name == selected_name)
    
    vsel <- selected_vertex()
    if (!is.null(vsel) && vsel$roi == selected_name) {
      # Move only the selected vertex
      target_row <- roi_idx[vsel$idx]
      if (length(target_row) == 1 && !is.na(target_row)) {
        roi_data$x[target_row] <- roi_data$x[target_row] + dx
        roi_data$y[target_row] <- roi_data$y[target_row] + dy
      }
    } else {
      # Move the entire ROI
      roi_data$x[roi_idx] <- roi_data$x[roi_idx] + dx
      roi_data$y[roi_idx] <- roi_data$y[roi_idx] + dy
    }
    
    roi_list(roi_data)
    
    dir_lab <- c("37"="left","39"="right","38"="up","40"="down")
    showNotification(
      paste("Moved", selected_name, dir_lab[as.character(key_code)], "by", step, "px"),
      type = "message", duration = 1
    )
  })
  
  observeEvent(input$vertex_next, {
    req(selected_roi())
    rd <- roi_list()
    sel <- rd[rd$Name == selected_roi(), ]
    n <- nrow(sel)
    if (n < 1) return(NULL)
    cur <- selected_vertex()
    k <- if (is.null(cur) || cur$roi != selected_roi()) 1 else ((cur$idx %% n) + 1)
    selected_vertex(list(roi = selected_roi(), idx = k))
    showNotification(paste("Vertex", k, "selected"), type = "message", duration = 1.5)
  })
  
  # FIXED: Complete save_roi functionality with video file info and correct notification type
  # FIXED: Complete save_roi functionality with proper column matching
  observeEvent(input$save_roi, {
    req(input$shape_label)
    req(nrow(polygon_points()) >= 3)  # Need at least 3 points for polygon
    req(input$video)  # Need video file for metadata
    
    current <- polygon_points()
    current$Name <- input$shape_label
    dims <- ref_dims()
    current$FrameWidth <- dims[1]
    current$FrameHeight <- dims[2]
    
    # Add video file information
    current$VideoFile <- basename(input$video$name)
    current$VideoPath <- input$video$name
    current$FrameTime_sec <- input$frame_sec
    current$CreatedDate <- Sys.Date()
    current$CreatedTime <- format(Sys.time(), "%H:%M:%S")
    
    # Get existing ROI data
    existing_roi <- roi_list()
    
    # Handle the case where this is the first ROI being saved
    if (nrow(existing_roi) == 0) {
      # If no existing ROIs, just use the current data
      roi_list(current)
    } else {
      # Make sure both data frames have the same columns
      all_cols <- union(names(existing_roi), names(current))
      
      # Add missing columns to existing_roi with appropriate default values
      for (col in setdiff(all_cols, names(existing_roi))) {
        if (col %in% c("FrameWidth", "FrameHeight", "FrameTime_sec")) {
          existing_roi[[col]] <- NA_real_
        } else if (col %in% c("VideoFile", "VideoPath", "Name")) {
          existing_roi[[col]] <- NA_character_
        } else if (col == "CreatedDate") {
          existing_roi[[col]] <- as.Date(NA)
        } else if (col == "CreatedTime") {
          existing_roi[[col]] <- NA_character_
        } else {
          existing_roi[[col]] <- NA
        }
      }
      
      # Add missing columns to current with appropriate default values
      for (col in setdiff(all_cols, names(current))) {
        if (col %in% c("FrameWidth", "FrameHeight", "FrameTime_sec")) {
          current[[col]] <- NA_real_
        } else if (col %in% c("VideoFile", "VideoPath", "Name")) {
          current[[col]] <- NA_character_
        } else if (col == "CreatedDate") {
          current[[col]] <- as.Date(NA)
        } else if (col == "CreatedTime") {
          current[[col]] <- NA_character_
        } else {
          current[[col]] <- NA
        }
      }
      
      # Reorder columns to match
      current <- current[names(existing_roi)]
      
      # Now combine them
      roi_list(rbind(existing_roi, current))
    }
    
    # Reset the polygon and clear the input
    polygon_points(data.frame(x = numeric(), y = numeric()))
    updateTextInput(session, "shape_label", value = "")
    
    showNotification(paste("ROI", input$shape_label, "saved successfully with video metadata!"), 
                     type = "message", duration = 3)
  })
  
  # Add ROI download functionality
  # Add ROI download functionality with calibration data
  output$download_rois <- downloadHandler(
    filename = function() {
      video_name <- if (!is.null(input$video)) {
        tools::file_path_sans_ext(basename(input$video$name))
      } else {
        "roi_definitions"
      }
      paste0(video_name, "_ROI_definitions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      roi_data <- roi_list()
      
      if (nrow(roi_data) > 0) {
        # Add calibration metadata to each row
        roi_data$Calib_PxPerCm <- if (!is.na(calib_px_per_cm())) calib_px_per_cm() else NA
        roi_data$Calib_LineLength_px <- if (!is.na(calib_pixlen())) calib_pixlen() else NA
        roi_data$Calib_RealLength_cm <- if (!is.na(calib_length_cm_applied())) calib_length_cm_applied() else NA
        roi_data$Calib_Units <- if (!is.na(calib_units_applied())) calib_units_applied() else NA
        
        # Add the two calibration points if they exist
        pts <- calib_pts()
        if (nrow(pts) == 2) {
          roi_data$Calib_Point1_X <- pts$x[1]
          roi_data$Calib_Point1_Y <- pts$y[1]
          roi_data$Calib_Point2_X <- pts$x[2]
          roi_data$Calib_Point2_Y <- pts$y[2]
        } else {
          roi_data$Calib_Point1_X <- NA
          roi_data$Calib_Point1_Y <- NA
          roi_data$Calib_Point2_X <- NA
          roi_data$Calib_Point2_Y <- NA
        }
        
        write.csv(roi_data, file, row.names = FALSE)
        
        cat("\n=== ROI + Calibration Saved ===\n")
        cat("File:", file, "\n")
        cat("Calibration: px/cm =", calib_px_per_cm(), "\n")
        cat("================================\n\n")
        
      } else {
        write.csv(data.frame(Message = "No ROI definitions to download"), file, row.names = FALSE)
      }
    }
  )
  
  # ROI status display
  output$roi_status_text <- renderText({
    roi_data <- roi_list()
    selected <- selected_roi()
    polygon_pts <- nrow(polygon_points())
    
    if (!is.null(selected)) {
      paste("✅ Selected ROI:", selected, "- Use arrow keys to move, or click Delete to remove")
    } else if (polygon_pts > 0) {
      paste("🔨 Creating new ROI:", polygon_pts, "points added - Click to add more, use Remove Last Point to undo, then enter name and Save")
    } else if (nrow(roi_data) > 0) {
      roi_names <- unique(roi_data$Name)
      paste("📍 Available ROIs:", paste(roi_names, collapse = ", "), "- Click inside an ROI to select it")
    } else if (!is.null(frame_path())) {
      "🎯 Ready to create ROIs - Click on the frame to start drawing a polygon"
    } else {
      "📹 Upload a video and extract a frame to begin creating ROIs"
    }
  })
  
  # FIXED: Delete selected ROI with correct notification type
  observeEvent(input$delete_roi, {
    req(selected_roi())
    
    roi_data <- roi_list()
    selected_name <- selected_roi()
    
    # Remove all rows with the selected ROI name
    roi_data <- roi_data[roi_data$Name != selected_name, ]
    roi_list(roi_data)
    
    # Clear selection
    selected_roi(NULL)
    
    showNotification(paste("ROI", selected_name, "deleted successfully!"), 
                     type = "warning", duration = 3)
  })
  
  # Center the selected ROI on the chosen target ROI (centroid-to-centroid)
  observeEvent(input$align_center, {
    rd <- roi_list()
    sel <- selected_roi()
    tgt <- input$align_target
    
    validate(
      need(nrow(rd) > 0, "No ROIs available."),
      need(!is.null(sel), "Select the ROI you want to move (click inside it first)."),
      need(!is.null(tgt) && nzchar(tgt), "Pick a target ROI to align to."),
      need(sel != tgt, "Source and target are the same.")
    )
    
    src_poly <- rd[rd$Name == sel, c("x","y")]
    tgt_poly <- rd[rd$Name == tgt, c("x","y")]
    validate(
      need(nrow(src_poly) >= 3, "Selected ROI needs at least 3 points."),
      need(nrow(tgt_poly) >= 3, "Target ROI needs at least 3 points.")
    )
    
    src_c <- poly_centroid(src_poly$x, src_poly$y)
    tgt_c <- poly_centroid(tgt_poly$x, tgt_poly$y)
    
    dx <- tgt_c[1] - src_c[1]
    dy <- tgt_c[2] - src_c[2]
    
    roi_list(move_roi_by(rd, sel, dx, dy))
    showNotification(
      paste0("Aligned '", sel, "' center to '", tgt, "' (Δx=", round(dx,1),
             ", Δy=", round(dy,1), ")."),
      type = "message", duration = 3
    )
  })
  
  
  # NEW: Remove last point from current polygon
  observeEvent(input$undo_point, {
    current_points <- polygon_points()
    if (nrow(current_points) > 0) {
      # Remove the last row
      updated_points <- current_points[-nrow(current_points), , drop = FALSE]
      polygon_points(updated_points)
      
      showNotification(paste("Removed last point -", nrow(updated_points), "points remaining"), 
                       type = "message", duration = 2)
    } else {
      showNotification("No points to remove", type = "warning", duration = 2)
    }
  })
  
  # NEW: Clear all points from current polygon
  observeEvent(input$clear_points, {
    current_points <- polygon_points()
    if (nrow(current_points) > 0) {
      polygon_points(data.frame(x = numeric(), y = numeric()))
      showNotification(paste("Cleared all", nrow(current_points), "points"), 
                       type = "message", duration = 2)
    } else {
      showNotification("No points to clear", type = "warning", duration = 2)
    }
  })
  
  # ENHANCED: Analysis with frame-by-frame data generation and ROI coverage debugging
  observeEvent(input$analyze, {
    req(input$h5file)
    req(input$video)
    
    tryCatch({
      # Read H5 file and get basic info
      h5 <- H5File$new(input$h5file$datapath, mode = "r")
      tracks <- h5[["tracks"]]$read()
      
      # Get node names with improved handling
      if ("node_names" %in% names(h5)) {
        node_names <- h5[["node_names"]]$read()
        if (is.list(node_names)) {
          node_names <- sapply(node_names, function(x) {
            if (is.raw(x)) {
              rawToChar(x[x != 0])
            } else {
              as.character(x)
            }
          })
        } else {
          node_names <- sapply(node_names, function(x) gsub("\\x00", "", x))
        }
        node_names <- trimws(node_names)
      } else {
        stop("No node_names found in H5 file")
      }
      
      h5$close_all()
      
      # Get video info
      fps <- extract_fps(input$video$datapath)
      video_fps(fps)
      
      # Prepare ROI definitions
      roi_data <- roi_list()
      if (nrow(roi_data) > 0) {
        roi_split <- split(roi_data[,c("x","y")], roi_data$Name)
        roi_split <- lapply(roi_split, function(df) as.matrix(df))
      } else {
        roi_split <- list()
      }
      
      # NEW: Generate frame-by-frame data
      if (length(roi_split) > 0) {
        cat("Generating frame-by-frame ROI data...\n")
        frame_data <- generate_frame_roi_data(input$h5file$datapath, roi_split, fps)
        frame_roi_data(frame_data)
        cat("Frame-by-frame data generated with", nrow(frame_data), "rows\n")
      }
      
      # NEW: Add ROI coverage debugging
      coverage_debug_results <- list()
      if (length(roi_split) > 0) {
        for (node in node_names) {
          coverage_debug <- debug_roi_coverage(input$h5file$datapath, node, roi_split, fps)
          coverage_debug_results[[node]] <- coverage_debug
          cat("ROI Coverage Debug for", node, ":\n")
          print(coverage_debug$summary)
          cat("\n")
        }
      }
      roi_coverage_debug(coverage_debug_results)
      
      # NEW: Generate outside ROI analysis for each node
      if (length(roi_split) > 0) {
        outside_roi_results <- list()
        
        for (node in node_names) {
          cat("Analyzing outside ROI positions for node:", node, "\n")
          outside_analysis <- generate_outside_roi_heatmap(input$h5file$datapath, node, roi_split, fps)
          outside_roi_results[[node]] <- outside_analysis
          
          # Print summary
          summary <- outside_analysis$summary
          cat("Node", node, "- Outside ROI:", summary$outside_roi_frames, "frames (", summary$outside_roi_percent, "%)\n")
        }
        
        outside_roi_data(outside_roi_results)
      }
      
      # Run analysis for each node
      all_results <- list()
      node_summary <- list()
      all_coordinates <- list()  # Store coordinates for plotting
      bin_size <- input$bin_size
      
      for (node in node_names) {
        cat("Processing node:", node, "\n")
        coords <- merge_tracks(input$h5file$datapath, node)
        
        # Store coordinates for plotting
        coords_df <- data.frame(
          Frame = 1:nrow(coords),
          Time_sec = (1:nrow(coords)) / fps,
          x = coords[,1],
          y = coords[,2],
          Node = node,
          Valid = !is.na(coords[,1]) & !is.na(coords[,2])
        )
        
        # Distance & speed series
        dist_series <- compute_distance_series(
          coords,
          fps = fps,
          px_per_cm = px_per_cm(),                      # ← use reactive, not input$px_per_cm
          max_interp_gap = if (is.null(input$interp_gap)) 3 else input$interp_gap
        )
        
        
        coords_df$Step_px      <- dist_series$step_px
        coords_df$Speed_px_s   <- dist_series$speed_px_s
        coords_df$CumDist_px   <- dist_series$cum_px
        coords_df$Step_cm      <- dist_series$step_cm
        coords_df$Speed_cm_s   <- dist_series$speed_cm_s
        coords_df$CumDist_cm   <- dist_series$cum_cm
        
        # Binned distance/speed aligned to bin_size
        dist_bin <- bin_distance_speed(
          time_sec = coords_df$Time_sec,
          step_px  = coords_df$Step_px,
          step_cm  = coords_df$Step_cm,
          speed_cm_s = coords_df$Speed_cm_s,
          fps = fps,
          bin_size = bin_size
        )
        dist_bin$Node <- node
        
        all_coordinates[[node]] <- coords_df
        
        # Count valid coordinates
        valid_coords <- sum(!is.na(coords[,1]))
        total_frames <- nrow(coords)
        total_dist_px <- sum(coords_df$Step_px, na.rm = TRUE)
        total_dist_cm <- if (all(is.na(coords_df$Step_cm))) NA_real_ else sum(coords_df$Step_cm, na.rm = TRUE)
        
        node_summary[[node]] <- data.frame(
          Node = node,
          ValidCoords = valid_coords,
          TotalFrames = total_frames,
          PercentMissing = round((total_frames - valid_coords) / total_frames * 100, 1),
          TotalDistance_px = round(total_dist_px, 1),
          TotalDistance_cm = if (is.na(total_dist_cm)) NA else round(total_dist_cm, 2)
        )
        
        
        if (length(roi_split) > 0) {
          res <- compute_roi_occupancy(coords, roi_split, fps, bin_size)
          # Merge distance bins onto each Bin for this node
          res <- res %>%
            left_join(dist_bin, by = c("Bin" = "bin")) %>%
            mutate(Node = node, .before = 1)
          all_results[[node]] <- res
       
           } else {
          # If no ROIs, still keep distance bins as a result table
          res <- dist_bin %>%
            transmute(Bin = bin,
                      ROI = "AllSpace",
                      Frames = NA_integer_,
                      Time_sec = NA_real_,
                      Bin_Start, Bin_End,
                      Distance_px, Distance_cm, MeanSpeed_cm_s,
                      Node = node)
          all_results[[node]] <- res
        }
      }
      
      # Store coordinates for plotting
      if (length(all_coordinates) > 0) {
        all_coords_df <- bind_rows(all_coordinates)
        node_coordinates(all_coords_df)
        
        # Update node choices for plotting
        updateSelectInput(session, "plot_node", 
                          choices = setNames(node_names, node_names),
                          selected = node_names[1])
      }
      
      # Combine results
      if (length(all_results) > 0) {
        full_results <- bind_rows(all_results)
        analysis_results(full_results)
      } else {
        analysis_results(data.frame())
      }
      
      # Combine node summary
      node_summary_df <- bind_rows(node_summary)
      
      # Show completion notification with FIXED type
      showNotification("Analysis completed successfully!", 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Analysis error:", e$message), 
                       type = "error", duration = 10)
      cat("Analysis error:", e$message, "\n")
    })
  })
  
  # Update debug output
  # Enhanced debug output with outside ROI analysis
  output$debug_console <- renderPrint({
    analysis_data <- analysis_results()
    frame_data <- frame_roi_data()
    coords_data <- node_coordinates()
    outside_data <- outside_roi_data()  # NEW
    
    debug_info <- list(
      FrameDims = ref_dims(),
      ImageDims = image_dimensions(),
      ROI_Count = nrow(roi_list()),
      Selected_ROI = selected_roi(),
      Current_Polygon_Points = nrow(polygon_points()),
      Analysis_Available = !is.null(analysis_data) && nrow(analysis_data) > 0,
      Frame_Data_Available = !is.null(frame_data) && nrow(frame_data) > 0
    )
    
    # Add detailed analysis breakdown if available
    if (!is.null(analysis_data) && nrow(analysis_data) > 0) {
      debug_info$Analysis_Details <- list(
        Total_Rows = nrow(analysis_data),
        Unique_Nodes = length(unique(analysis_data$Node)),
        Unique_Bins = length(unique(analysis_data$Bin))
      )
      
      # Add per-node breakdown showing missing vs outside ROI
      if (!is.null(coords_data) && nrow(coords_data) > 0) {
        node_breakdown <- coords_data %>%
          group_by(Node) %>%
          summarise(
            Total_Frames = n(),
            Missing_Frames = sum(!Valid),
            Valid_Frames = sum(Valid),
            Missing_Percent = round(sum(!Valid) / n() * 100, 2),
            .groups = 'drop'
          )
        
        debug_info$Node_Missing_Data <- node_breakdown
        
        # Calculate ROI coverage for each node if ROI data exists
        if (!is.null(frame_data)) {
          roi_cols <- names(frame_data)[grepl("^ROI_", names(frame_data))]
          if (length(roi_cols) > 0) {
            roi_coverage <- frame_data %>%
              group_by(Node) %>%
              summarise(
                Total_Valid_Frames = sum(Valid),
                Frames_In_Any_ROI = sum(if_any(all_of(roi_cols), ~ .x), na.rm = TRUE),
                Frames_Outside_All_ROIs = sum(Valid & !if_any(all_of(roi_cols), ~ .x), na.rm = TRUE),
                Outside_ROI_Percent = round(sum(Valid & !if_any(all_of(roi_cols), ~ .x), na.rm = TRUE) / sum(Valid) * 100, 2),
                .groups = 'drop'
              )
            
            debug_info$ROI_Coverage_Per_Node <- roi_coverage
          }
        }
      }
    }
    
    # Add frame data details
    if (!is.null(frame_data) && nrow(frame_data) > 0) {
      debug_info$Frame_Data_Details <- list(
        Total_Frames = nrow(frame_data),
        Unique_Nodes = length(unique(frame_data$Node)),
        Available_ROI_Columns = names(frame_data)[grepl("^ROI_", names(frame_data))],
        Video_FPS = video_fps()
      )
    }
    
    # NEW: Add outside ROI debug info
    if (!is.null(outside_data)) {
      debug_info$Outside_ROI_Analysis <- lapply(names(outside_data), function(node) {
        summary <- outside_data[[node]]$summary
        list(
          Node = node,
          Outside_ROI_Frames = summary$outside_roi_frames,
          Outside_ROI_Percent = summary$outside_roi_percent,
          Missing_Percent = summary$missing_percent,
          Inside_ROI_Percent = summary$inside_roi_percent
        )
      })
      names(debug_info$Outside_ROI_Analysis) <- names(outside_data)
    }
    
    # Add the key insight about the difference
    debug_info$KEY_INSIGHT <- list(
      Message = "MISSING DATA vs OUTSIDE ROI are different!",
      Missing_Data = "Frames where SLEAP could not detect the body part (coordinates are NA)",
      Outside_ROI = "Frames where body part was detected but is outside all defined ROIs",
      Expected_Relationship = "Total time = Missing time + Outside ROI time + Inside ROI time"
    )
    
    return(debug_info)
  })
  
  # Display frame with ROIs
  output$frameplot <- renderPlot({
    req(frame_path())
    
    img <- image_read(frame_path())
    dims <- image_info(img)
    
    par(xaxs = "i", yaxs = "i")   # no 4% padding on axes
    
    
    # Convert to plot
    plot(0, 0, type="n", xlim=c(0, dims$width), ylim=c(dims$height, 0), 
         xlab="X", ylab="Y", main="Frame with ROIs", asp = 1)
    
    # Add background image
    rasterImage(as.raster(img), 0, dims$height, dims$width, 0)
    
    # Draw saved ROIs
    roi_data <- roi_list()
    selected_name <- selected_roi()
    
    if (nrow(roi_data) > 0) {
      roi_names <- unique(roi_data$Name)
      for (i in seq_along(roi_names)) {
        roi_name <- roi_names[i]
        roi_points <- roi_data[roi_data$Name == roi_name, ]
        if (nrow(roi_points) >= 3) {
          color_idx <- (i - 1) %% length(roi_colors) + 1
          border_color <- roi_colors[color_idx]
          
          # Highlight selected ROI
          if (!is.null(selected_name) && roi_name == selected_name) {
            border_color <- "black"
            border_width <- 4
            fill_color <- adjustcolor("yellow", alpha.f = 0.4)
          } else {
            border_width <- 2
            fill_color <- adjustcolor(border_color, alpha.f = 0.3)
          }
          
          polygon(roi_points$x, roi_points$y, 
                  border = border_color, 
                  col = fill_color,
                  lwd = border_width)
          
          # Add ROI label
          text_color <- if (!is.null(selected_name) && roi_name == selected_name) "black" else border_color
          text(mean(roi_points$x), mean(roi_points$y), roi_name, 
               col = text_color, cex = 1.2, font = 2)
        }
      }
    }
    # Draw in-progress polygon (with "close here" hint on first vertex)
    cur <- polygon_points()
    if (nrow(cur) > 0) {
      points(cur$x, cur$y, col = "red", pch = 16, cex = 1.3)
      if (nrow(cur) > 1) {
        lines(cur$x, cur$y, col = "red", lwd = 2)
      }
      # ring on the first point to indicate closure target
      points(cur$x[1], cur$y[1], pch = 1, cex = 1.8, lwd = 2, col = "red")
    }
    
   
    # --- Calibration line overlay ---
    pts <- calib_pts()
    if (nrow(pts) >= 1) {
      points(pts$x, pts$y, col = "deepskyblue3", pch = 16, cex = 1.6)
    }
    if (nrow(pts) == 2) {
      segments(pts$x[1], pts$y[1], pts$x[2], pts$y[2], col = "deepskyblue3", lwd = 3)
      midx <- mean(pts$x); midy <- mean(pts$y)
      dpx <- sqrt((pts$x[2]-pts$x[1])^2 + (pts$y[2]-pts$y[1])^2)
      lab <- paste0(round(dpx,1), " px")
      rect(midx-30, midy-20, midx+30, midy+5, col=adjustcolor("white",0.7), border=NA)
      text(midx, midy-5, lab, col="black", cex=0.9, font=2)
    }
    # --- end calibration overlay ---
    
    # --- Vertex handles for the selected ROI (drawn last so they sit on top) ---
    sel_name <- selected_roi()
    if (!is.null(sel_name)) {
      rd <- roi_list()
      sel <- rd[rd$Name == sel_name, ]
      if (nrow(sel) > 0) {
        points(sel$x, sel$y, pch = 16, cex = 0.9, col = "black")
        points(sel$x, sel$y, pch = 1,  cex = 1.5, col = "white", lwd = 2)
        vsel <- selected_vertex()
        if (!is.null(vsel) && vsel$roi == sel_name && vsel$idx <= nrow(sel)) {
          points(sel$x[vsel$idx], sel$y[vsel$idx],
                 pch = 21, cex = 1.6, bg = "orange", col = "black", lwd = 2)
        }
      }
    }
    # --- end vertex handles ---
  })
  
  # Analysis availability check
  output$analysis_available <- reactive({
    !is.null(analysis_results()) && nrow(analysis_results()) > 0
  })
  outputOptions(output, "analysis_available", suspendWhenHidden = FALSE)
  
  # Show results preview
  output$results_preview <- renderTable({
    if (!is.null(analysis_results()) && nrow(analysis_results()) > 0) {
      head(analysis_results(), 20)
    } else {
      data.frame(Message = "No analysis results available")
    }
  })
  
  # H5 inspection output
  output$h5_inspection_available <- reactive({
    !is.null(h5_inspection_results())
  })
  outputOptions(output, "h5_inspection_available", suspendWhenHidden = FALSE)
  
  output$h5_inspection_output <- renderUI({
    req(h5_inspection_results())
    
    result <- h5_inspection_results()
    
    # Create status color based on result
    status_color <- switch(result$status,
                           "success" = "#28a745",
                           "warning" = "#ffc107", 
                           "error" = "#dc3545",
                           "#6c757d")
    
    status_bg <- switch(result$status,
                        "success" = "#d4edda",
                        "warning" = "#fff3cd",
                        "error" = "#f8d7da", 
                        "#f8f9fa")
    
    # Build the HTML output
    html_content <- tags$div(
      # Overall status
      tags$div(
        style = paste0("background-color: ", status_bg, "; border: 1px solid ", status_color, "; border-radius: 5px; padding: 10px; margin-bottom: 10px;"),
        tags$strong(style = paste0("color: ", status_color, ";"), result$overall)
      ),
      
      # File info
      tags$p(tags$strong("File: "), result$filename),
      
      if (length(result$datasets) > 0) {
        tags$p(tags$strong("Available datasets: "), paste(result$datasets, collapse = ", "))
      },
      
      # Issues/checks
      if (length(result$issues) > 0) {
        tags$div(
          tags$strong("Checks:"),
          tags$ul(
            lapply(result$issues, function(issue) {
              tags$li(HTML(issue))
            })
          )
        )
      },
      
      # Details
      if (length(result$details) > 0 && !is.null(result$details$tracks)) {
        tags$div(
          tags$strong("Track Details:"),
          tags$ul(
            tags$li(paste("Structure:", result$details$tracks$structure)),
            tags$li(paste("Shape:", paste(result$details$tracks$shape, collapse = " × "))),
            tags$li(paste("Frames:", result$details$tracks$n_frames)),
            if (!is.null(result$details$tracks$n_nodes)) {
              tags$li(paste("Body parts:", result$details$tracks$n_nodes))
            },
            if (!is.null(result$details$tracks$n_instances)) {
              tags$li(paste("Instances:", result$details$tracks$n_instances))
            }
          )
        )
      },
      
      if (!is.null(result$details$node_names)) {
        tags$div(
          tags$strong("Body Parts Found:"),
          tags$p(style = "font-family: monospace; background-color: #f8f9fa; padding: 5px; border-radius: 3px;",
                 paste(result$details$node_names, collapse = ", "))
        )
      }
    )
    
    return(html_content)
  })
  
  # One source of truth for trajectory binning
  # Compute breaks + human labels once
  traj_bins_spec <- reactive({
    req(node_coordinates(), input$plot_node)
    sel <- node_coordinates() %>% dplyr::filter(Node == input$plot_node, Valid)
    if (nrow(sel) == 0) return(NULL)
    bs <- suppressWarnings(as.numeric(input$traj_bin_sec)); if (is.na(bs) || bs <= 0) bs <- 60
    max_t <- max(sel$Time_sec, na.rm = TRUE)
    brks  <- seq(0, max_t + bs, by = bs)
    labs  <- paste0(sprintf("%.0f", head(brks, -1)), "–", sprintf("%.0f", tail(brks, -1)), " s")
    list(breaks = brks, labels = labs, bin_size = bs)
  })
  
  # Populate the single-bin dropdown with numeric values (1..K) and pretty labels
  observe({
    spec <- traj_bins_spec(); if (is.null(spec)) return()
    choices <- setNames(as.character(seq_along(spec$labels)), spec$labels)  # values: "1","2",..., labels: "0–30 s" etc.
    updateSelectInput(session, "traj_bin_pick", choices = choices, selected = "1")
  })
  
  
  #make plots!
  generate_plot <- function() {
    req(input$plot_node, input$plot_type)
    req(node_coordinates())
    
    coords_data <- node_coordinates()
    selected_coords <- coords_data[coords_data$Node == input$plot_node, ]
    
    img_dims <- image_dimensions()
    use_normalized_axes <- !is.null(input$normalize_axes) && input$normalize_axes && !any(is.na(img_dims))
    
    if (input$plot_type == "heatmap") {
      
      valid_coords <- selected_coords[selected_coords$Valid, ]
      if (nrow(valid_coords) > 0) {
        p <- ggplot(valid_coords, aes(x = x, y = y)) +
          stat_density_2d_filled(alpha = 0.8, contour_var = "ndensity") +
          scale_fill_viridis_d(name = "Density") +
          labs(title = paste("Position Heatmap -", input$plot_node),
               x = "X Position (pixels)", y = "Y Position (pixels)") +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"),
                axis.text = element_text(size = 10))
        
        p <- add_roi_overlays(p, input$traj_roi, roi_list(), roi_colors, add_labels = TRUE)
        
        if (use_normalized_axes) {
          p <- p +
            scale_x_continuous(limits = c(0, img_dims[1]), expand = c(0, 0)) +
            scale_y_reverse(limits = c(img_dims[2], 0), expand = c(0, 0)) +
            coord_fixed(ratio = 1)
        } else {
          p <- p + coord_fixed()
        }
      } else {
        p <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No valid coordinates for heatmap", size = 6) +
          theme_void()
      }
      
     } else if (input$plot_type == "traj") {
      sel <- selected_coords[selected_coords$Valid, ]
      if (nrow(sel) < 2) {
        p <- ggplot() + annotate("text", x=.5, y=.5, label="Not enough points for trajectory", size=6) + theme_void()
      } else {
        spec <- traj_bins_spec()
        brks <- spec$breaks; bs <- spec$bin_size
        
        # compute bin index once, numeric 1..K
        sel$BinIndex <- as.integer(cut(sel$Time_sec, breaks = brks, include.lowest = TRUE, right = FALSE))
        sel$TimeBin  <- factor(sel$BinIndex, levels = seq_along(spec$labels), labels = spec$labels)
        
        # optional ROI clipping (uses the global picker 'traj_roi')
        if (isTRUE(input$traj_clip_to_roi) && !is.null(input$traj_roi) && length(input$traj_roi) > 0) {
          rd <- roi_list(); inside_any <- rep(FALSE, nrow(sel))
          for (roi_name in input$traj_roi) {
            poly <- rd[rd$Name == roi_name, c("x","y")]
            if (nrow(poly) >= 3) inside_any <- inside_any | (sp::point.in.polygon(sel$x, sel$y, poly$x, poly$y) > 0)
          }
          sel <- sel[inside_any, , drop = FALSE]
        }
        
        mode <- if (is.null(input$traj_bin_mode)) "facet" else input$traj_bin_mode
        if (mode == "single" && !is.null(input$traj_bin_pick) && nzchar(input$traj_bin_pick)) {
          want_idx <- suppressWarnings(as.integer(input$traj_bin_pick))
          if (!is.na(want_idx)) sel <- sel[sel$BinIndex == want_idx, , drop = FALSE]
        }
        
        
        if (nrow(sel) < 2) {
          p <- ggplot() + annotate("text", x=.5, y=.5, label="No points in selected bin", size=6) + theme_void()
        } else {
          p <- ggplot(sel, aes(x = x, y = y, group = TimeBin)) +
            geom_path(linewidth = 0.6, alpha = 0.95, color = "red") +
            labs(title = paste("Trajectory —", input$plot_node)) +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, face = "bold"),
                  strip.text = element_text(size = 10))
          if (mode == "facet") p <- p + facet_wrap(~ TimeBin, ncol = 3)
          
          p <- add_roi_overlays(p, input$traj_roi, roi_list(), roi_colors, add_labels = TRUE)
          
          img_dims <- image_dimensions()
          if (isTRUE(input$traj_use_image_limits) && !any(is.na(img_dims))) {
            p <- p +
              scale_x_continuous(limits = c(0, img_dims[1]), expand = c(0,0)) +
              scale_y_reverse(limits = c(img_dims[2], 0), expand = c(0,0)) +
              coord_fixed(ratio = 1)
          } else {
            p <- p + coord_fixed()
          }
        }
      } 
    } else if (input$plot_type == "temporal_heatmap") {
      
      valid_coords <- selected_coords[selected_coords$Valid, ]
      if (nrow(valid_coords) > 0) {
        time_bin_size <- suppressWarnings(as.numeric(input$plot_time_bin)); if (is.na(time_bin_size) || time_bin_size <= 0) time_bin_size <- 60
        max_time <- max(valid_coords$Time_sec, na.rm = TRUE)
        time_breaks <- seq(0, max_time + time_bin_size, by = time_bin_size)
        valid_coords$TimeBin <- cut(valid_coords$Time_sec, breaks = time_breaks, include.lowest = TRUE, right = FALSE)
        valid_coords <- valid_coords[!is.na(valid_coords$TimeBin), , drop = FALSE]
        
        if (nrow(valid_coords) > 0) {
          valid_coords$TimePeriod <- paste0(
            "Period ", as.numeric(valid_coords$TimeBin),
            " (", round((as.numeric(valid_coords$TimeBin) - 1) * time_bin_size, 1),
            "-",  round(as.numeric(valid_coords$TimeBin) * time_bin_size, 1), "s)"
          )
          
          spatial_bins   <- if (!is.null(input$heatmap_bins))   input$heatmap_bins   else 25
          heatmap_method <- if (!is.null(input$heatmap_method)) input$heatmap_method else "density"
          
          if (heatmap_method == "density") {
            p <- ggplot(valid_coords, aes(x = x, y = y)) +
              stat_density_2d_filled(alpha = 0.8, contour_var = "ndensity", bins = spatial_bins) +
              scale_fill_viridis_d(name = "Density") +
              facet_wrap(~ TimePeriod, ncol = 3) +
              labs(title = paste("Temporal Position Heatmap -", input$plot_node),
                   subtitle = paste("Time bins:", time_bin_size, "seconds"),
                   x = "X Position (pixels)", y = "Y Position (pixels)") +
              theme_minimal() +
              theme(plot.title = element_text(size = 14, face = "bold"),
                    plot.subtitle = element_text(size = 12),
                    axis.text = element_text(size = 8),
                    strip.text = element_text(size = 10))
          } else {
            p <- ggplot(valid_coords, aes(x = x, y = y)) +
              geom_bin2d(bins = spatial_bins, alpha = 0.8) +
              scale_fill_viridis_c(name = "Count") +
              facet_wrap(~ TimePeriod, ncol = 3) +
              labs(title = paste("Temporal Position Count Map -", input$plot_node),
                   subtitle = paste("Time bins:", time_bin_size, "seconds"),
                   x = "X Position (pixels)", y = "Y Position (pixels)") +
              theme_minimal() +
              theme(plot.title = element_text(size = 14, face = "bold"),
                    plot.subtitle = element_text(size = 12),
                    axis.text = element_text(size = 8),
                    strip.text = element_text(size = 10))
          }
          
          p <- add_roi_overlays(p, input$traj_roi, roi_list(), roi_colors, add_labels = TRUE)
          
          if (use_normalized_axes) {
            p <- p +
              scale_x_continuous(limits = c(0, img_dims[1]), expand = c(0, 0)) +
              scale_y_reverse(limits = c(img_dims[2], 0), expand = c(0, 0)) +
              coord_fixed(ratio = 1)
          } else {
            p <- p + coord_fixed()
          }
        } else {
          p <- ggplot() + annotate("text", x=.5, y=.5, label="No valid coordinates for temporal heatmap", size=6) + theme_void()
        }
      } else {
        p <- ggplot() + annotate("text", x=.5, y=.5, label="No valid coordinates for temporal heatmap", size=6) + theme_void()
      }
      
    } else if (input$plot_type == "cumdist") {
      valid <- selected_coords[!is.na(selected_coords$CumDist_px), ]
      if (nrow(valid) > 1) {
        unit_is_cm <- !all(is.na(valid$CumDist_cm))
        yvar <- if (unit_is_cm) "CumDist_cm" else "CumDist_px"
        ylab <- if (unit_is_cm) "Cumulative Distance (cm)" else "Cumulative Distance (px)"
        p <- ggplot(valid, aes(x = Time_sec, y = .data[[yvar]])) +
          geom_line(linewidth = 1) +
          labs(title = paste("Cumulative Distance -", input$plot_node), x = "Time (s)", y = ylab) +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"))
      } else {
        p <- ggplot() + annotate("text", x=.5, y=.5, label="Not enough data for cumulative distance", size=6) + theme_void()
      }
      
    } else if (input$plot_type == "speed") {
      if (is.na(px_per_cm())) {
        p <- ggplot() + annotate("text", x=.5, y=.5, label="Calibrate (📏) to view speed in cm/s", size=5) + theme_void()
      } else {
        valid <- selected_coords[!is.na(selected_coords$Speed_cm_s), ]
        if (nrow(valid) > 1) {
          p <- ggplot(valid, aes(x = Time_sec, y = Speed_cm_s)) +
            geom_line(alpha = 0.85) +
            labs(title = paste("Instantaneous Speed —", input$plot_node), x = "Time (s)", y = "Speed (cm/s)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, face = "bold"))
        } else {
          p <- ggplot() + annotate("text", x=.5, y=.5, label="Not enough data for speed plot", size=6) + theme_void()
        }
      }
      
    } else if (input$plot_type == "dist_bin") {
      req(analysis_results(), input$bin_size)
      ar <- analysis_results()
      if (nrow(ar) == 0) {
        p <- ggplot() + annotate("text", x=.5, y=.5, label="No analysis results", size=6) + theme_void()
      } else {
        df <- ar %>%
          dplyr::filter(Node == input$plot_node) %>%
          dplyr::group_by(Bin) %>%
          dplyr::summarise(
            Distance_cm = {val <- suppressWarnings(max(Distance_cm, na.rm = TRUE)); if (is.infinite(val)) NA_real_ else val},
            Distance_px = {val <- suppressWarnings(max(Distance_px, na.rm = TRUE)); if (is.infinite(val)) NA_real_ else val},
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            Bin_Start = Bin * input$bin_size,
            Bin_End   = (Bin + 1) * input$bin_size,
            TimeMid   = (Bin_Start + Bin_End) / 2,
            BinWidth  = pmax(Bin_End - Bin_Start, 0.1)
          ) %>% dplyr::arrange(Bin)
        
        use_cm <- !all(is.na(df$Distance_cm))
        yvar <- if (use_cm) "Distance_cm" else "Distance_px"
        ylab <- if (use_cm) "Distance per bin (cm)" else "Distance per bin (px)"
        
        if (nrow(df) > 0) {
          p <- ggplot(df, aes(x = TimeMid, y = .data[[yvar]])) +
            geom_col(width = df$BinWidth, alpha = 0.9) +
            labs(title = paste("Distance per Bin —", input$plot_node),
                 subtitle = paste0("Bin size: ", input$bin_size, " s"),
                 x = "Time (s)", y = ylab) +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, face = "bold"),
                  axis.text = element_text(size = 10))
        } else {
          p <- ggplot() + annotate("text", x=.5, y=.5, label="No bins to show", size=6) + theme_void()
        }
      }
      
    } else if (input$plot_type == "raster") {
      tb <- suppressWarnings(as.numeric(input$plot_time_bin)); if (is.na(tb) || tb <= 0) tb <- 1
      time_bins <- seq(0, max(selected_coords$Time_sec, na.rm = TRUE), by = tb)
      selected_coords$TimeBin <- cut(selected_coords$Time_sec, breaks = time_bins, include.lowest = TRUE)
      raster_data <- selected_coords %>%
        dplyr::group_by(TimeBin) %>%
        dplyr::summarise(Detection = any(Valid), Time_sec = mean(Time_sec, na.rm = TRUE), .groups = 'drop') %>%
        dplyr::mutate(Status = ifelse(Detection, "Detected", "Missing"), y_pos = 1)
      
      p <- ggplot(raster_data, aes(x = Time_sec, y = y_pos, fill = Status)) +
        geom_tile(height = 0.8, color = "white", size = 0.1) +
        scale_fill_manual(values = c("Detected" = "#2E86C1", "Missing" = "black")) +
        labs(title = paste("Detection Raster -", input$plot_node, paste0("(", tb, "s bins)")),
             x = "Time (seconds)", y = "") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "bottom") +
        scale_y_continuous(expand = c(0, 0))
      
    } else if (input$plot_type == "missing") {
      all_coords <- node_coordinates()
      missing_summary <- all_coords %>%
        dplyr::group_by(Node) %>%
        dplyr::summarise(Total = n(), Missing = sum(!Valid), PercentMissing = round(Missing / Total * 100, 1), .groups = 'drop')
      missing_summary$Highlight <- missing_summary$Node == input$plot_node
      p <- ggplot(missing_summary, aes(x = reorder(Node, PercentMissing), y = PercentMissing, fill = Highlight)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        scale_fill_manual(values = c("FALSE" = "#85C1E9", "TRUE" = "#2E86C1"), guide = "none") +
        geom_text(aes(label = paste0(PercentMissing, "%")), hjust = -0.1, size = 3.5, fontface = "bold") +
        coord_flip() +
        labs(title = "Missing Data Percentage by Body Part", x = "Body Part", y = "Percentage Missing (%)") +
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 10)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      
    } else if (input$plot_type == "roi_time") {
      analysis_data <- analysis_results()
      if (!is.null(analysis_data) && nrow(analysis_data) > 0) {
        roi_summary <- analysis_data %>%
          dplyr::filter(Node == input$plot_node) %>%
          dplyr::group_by(ROI) %>%
          dplyr::summarise(TotalTime = sum(Time_sec, na.rm = TRUE), .groups = 'drop') %>%
          dplyr::arrange(desc(TotalTime))
        
        if (nrow(roi_summary) > 0) {
          p <- ggplot(roi_summary, aes(x = reorder(ROI, TotalTime), y = TotalTime)) +
            geom_bar(stat = "identity", fill = "#2E86C1", alpha = 0.8) +
            geom_text(aes(label = paste0(round(TotalTime, 1), "s")), hjust = -0.1, size = 3.5, fontface = "bold") +
            coord_flip() +
            labs(title = paste("Time Spent in Each ROI -", input$plot_node), x = "ROI", y = "Time Spent (seconds)") +
            theme_minimal() +
            theme(plot.title = element_text(size = 14, face = "bold"), axis.text = element_text(size = 10)) +
            scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
        } else {
          p <- ggplot() + annotate("text", x=.5, y=.5, label="No ROI data available", size=6) + theme_void()
        }
      } else {
        p <- ggplot() + annotate("text", x=.5, y=.5, label="No analysis results available", size=6) + theme_void()
      }
    }
    
    return(p)
  }
  
  
  observeEvent(input$update_plot, {
    plot_obj <- generate_plot()
    current_plot(plot_obj)
    
    output$analysis_plot <- renderPlot({
      plot_obj
    })
  })
  
  # Auto-update plot when node selection changes
  observeEvent(input$plot_node, {
    # Validate inputs exist and have content
    if (is.null(input$plot_node) || 
        length(input$plot_node) == 0 || 
        input$plot_node == "" ||
        is.null(node_coordinates()) ||
        nrow(node_coordinates()) == 0) {
      return(NULL)
    }
    
    tryCatch({
      plot_obj <- generate_plot()
      current_plot(plot_obj)
      
      output$analysis_plot <- renderPlot({
        plot_obj
      })
    }, error = function(e) {
      cat("Error updating plot after node change:", e$message, "\n")
    })
  })
  
  # Auto-update plot when plot type changes
  observeEvent(input$plot_type, {
    # Validate inputs exist and have content
    if (is.null(input$plot_node) || 
        length(input$plot_node) == 0 || 
        input$plot_node == "" ||
        is.null(input$plot_type) ||
        length(input$plot_type) == 0 ||
        is.null(node_coordinates()) ||
        nrow(node_coordinates()) == 0) {
      return(NULL)
    }
    
    tryCatch({
      plot_obj <- generate_plot()
      current_plot(plot_obj)
      
      output$analysis_plot <- renderPlot({
        plot_obj
      })
    }, error = function(e) {
      cat("Error updating plot after type change:", e$message, "\n")
    })
  })
  
  observeEvent(input$mode_select, {
    edit_mode("select")
    selected_vertex(NULL)
    showNotification("Mode: Select / Move — click inside a shape to select; near a corner to edit a vertex.", type="message")
  })
  
  observeEvent(input$mode_shape, {
    edit_mode("shape")
    selected_vertex(NULL)
    polygon_points(data.frame(x=numeric(), y=numeric()))
    showNotification("Mode: Place Shape — click once on the frame to place the selected box/rectangle/circle.", type="message")
  })
  
  observeEvent(input$mode_poly, {
    edit_mode("poly")
    selected_roi(NULL); selected_vertex(NULL)
    polygon_points(data.frame(x=numeric(), y=numeric()))
    showNotification("Mode: Draw Polygon — clicks add vertices. Name it and click Save Current Poly when done.", type="message")
  })
  
  
  # NEW: Plot export functionality
  output$export_plot <- downloadHandler(
    filename = function() {
      video_name <- if (!is.null(input$video)) {
        tools::file_path_sans_ext(basename(input$video$name))
      } else {
        "behavior_plot"
      }
      
      plot_type_name <- switch(input$plot_type,
                               "heatmap" = "heatmap",
                               "temporal_heatmap" = paste0("temporal_heatmap_", input$plot_time_bin, "s"),
                               "outside_roi_heatmap" = "outside_roi_heatmap",
                               "raster" = paste0("raster_", input$plot_time_bin, "s"),
                               "missing" = "missing_data",
                               "roi_time" = "roi_time")
      
      node_name <- gsub("[^A-Za-z0-9_]", "_", input$plot_node)
      
      paste0(video_name, "_", node_name, "_", plot_type_name, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      if (!is.null(current_plot())) {
        # Wider for temporal heatmap OR faceted trajectory
        is_traj_facet <- identical(input$plot_type, "traj") && identical(input$traj_bin_mode, "facet")
        if (identical(input$plot_type, "temporal_heatmap") || is_traj_facet) {
          ggsave(file, current_plot(), width = 15, height = 10, dpi = 300, bg = "white")
        } else {
          ggsave(file, current_plot(), width = 10, height = 6, dpi = 300, bg = "white")
        }
      }
      else {
        # Create a placeholder if no plot available
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = "No plot available for export", size = 8) +
          theme_void()
        ggsave(file, p, width = 10, height = 6, dpi = 300, bg = "white")
      }
    }
  )
  
  output$export_plot_pdf <- downloadHandler(
    filename = function() {
      video_name <- if (!is.null(input$video)) tools::file_path_sans_ext(basename(input$video$name)) else "behavior_plot"
      plot_type_name <- switch(input$plot_type,
                               "heatmap" = "heatmap",
                               "temporal_heatmap" = paste0("temporal_heatmap_", input$plot_time_bin, "s"),
                               "outside_roi_heatmap" = "outside_roi_heatmap",
                               "raster" = paste0("raster_", input$plot_time_bin, "s"),
                               "missing" = "missing_data",
                               "roi_time" = "roi_time",
                               "dist_bin" = paste0("distbin_", input$bin_size, "s"),
                               "traj" = if (identical(input$traj_bin_mode, "facet")) "traj_facet" else "traj_single")
      node_name <- gsub("[^A-Za-z0-9_]", "_", input$plot_node)
      paste0(video_name, "_", node_name, "_", plot_type_name, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      p <- current_plot()
      if (is.null(p)) {
        p <- ggplot() + annotate("text", x=.5, y=.5, label="No plot available for export", size=8) + theme_void()
      }
      # Wider page for faceted heatmaps/trajectories
      is_traj_facet <- identical(input$plot_type, "traj") && identical(input$traj_bin_mode, "facet")
      w <- if (identical(input$plot_type, "temporal_heatmap") || is_traj_facet) 15 else 10
      h <- if (identical(input$plot_type, "temporal_heatmap") || is_traj_facet) 10 else 6
      ggsave(filename = file, plot = p, width = w, height = h, units = "in", device = cairo_pdf, bg = "white")
    }
  )
  
  
  # NEW: Frame-by-frame data export
  output$downloadFrameData <- downloadHandler(
    filename = function() {
      video_name <- if (!is.null(input$video)) {
        tools::file_path_sans_ext(basename(input$video$name))
      } else {
        "frame_data"
      }
      paste0(video_name, "_frame_by_frame_ROI_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      frame_data <- frame_roi_data()
      if (!is.null(frame_data) && nrow(frame_data) > 0) {
        # Add additional metadata columns
        frame_data$VideoFile <- if (!is.null(input$video)) basename(input$video$name) else "Unknown"
        frame_data$AnalysisDate <- Sys.Date()
        frame_data$AnalysisTime <- format(Sys.time(), "%H:%M:%S")
        frame_data$BinSize_sec <- input$bin_size
        frame_data$VideoFPS <- video_fps()
        frame_data$ImageWidth <- if (!any(is.na(image_dimensions()))) image_dimensions()[1] else NA
        frame_data$ImageHeight <- if (!any(is.na(image_dimensions()))) image_dimensions()[2] else NA
        
        # Reorder columns for better readability
        col_order <- c("VideoFile", "Frame", "Time_sec", "Node", "X_coord", "Y_coord", "Valid", "Step_px", "Speed_px_s", "CumDist_px",
                       "Step_cm", "Speed_cm_s", "CumDist_cm")
        roi_cols <- names(frame_data)[grepl("^ROI_", names(frame_data))]
        metadata_cols <- c("AnalysisDate", "AnalysisTime", "BinSize_sec", "VideoFPS", "ImageWidth", "ImageHeight")
        
        final_order <- c(col_order, roi_cols, metadata_cols)
        frame_data <- frame_data[, final_order[final_order %in% names(frame_data)]]
        
        frame_data$CalibLength_input_units <- calib_units_applied()
        frame_data$CalibLength_input_cm    <- calib_length_cm_applied()
        frame_data$PxPerCm                 <- px_per_cm()
       
        write.csv(frame_data, file, row.names = FALSE)
        
        cat("Exported frame data with", nrow(frame_data), "rows and", ncol(frame_data), "columns\n")
      } else {
        # Create informative message if no data
        error_data <- data.frame(
          Message = "No frame-by-frame data available",
          Note = "Please run analysis first with ROIs defined",
          Timestamp = Sys.time()
        )
        write.csv(error_data, file, row.names = FALSE)
      }
    }
  )
  
  # Standard analysis results download handler
  output$downloadResults <- downloadHandler(
    filename = function() {
      video_name <- if (!is.null(input$video)) {
        tools::file_path_sans_ext(basename(input$video$name))
      } else {
        "behavior_analysis"
      }
      paste0(video_name, "_binned_analysis_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(analysis_results()) && nrow(analysis_results()) > 0) {
        results_data <- analysis_results()
        
        # Add metadata
        results_data$VideoFile <- if (!is.null(input$video)) basename(input$video$name) else "Unknown"
        results_data$AnalysisDate <- Sys.Date()
        results_data$AnalysisTime <- format(Sys.time(), "%H:%M:%S")
        results_data$VideoFPS <- video_fps()
        results_data$ImageWidth <- if (!any(is.na(image_dimensions()))) image_dimensions()[1] else NA
        results_data$ImageHeight <- if (!any(is.na(image_dimensions()))) image_dimensions()[2] else NA
        results_data$CalibLength_input_units <- calib_units_applied()
        results_data$CalibLength_input_cm    <- calib_length_cm_applied()
        results_data$PxPerCm                 <- px_per_cm()
        
        write.csv(results_data, file, row.names = FALSE)
      } else {
        write.csv(data.frame(Message = "No binned analysis results to download"), file, row.names = FALSE)
      }
    }
  )
}

# Launch the application
shinyApp(ui, server)