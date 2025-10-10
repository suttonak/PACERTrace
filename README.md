# PACERTrace v1.0.0 — README

Track animal position and behavior from SLEAP `.h5` files with a point‑and‑click Shiny app for defining ROIs, calibrating pixels to centimeters, and exporting analysis and visualizations.

---

## ✨ What PACERTrace does

* Loads SLEAP tracking output (`.h5`) and the corresponding video frame.
* Lets you **calibrate** (px/cm) using a two‑point line on the frame.
* Provides a fast **ROI builder** (boxes, rectangles, circles, or freehand polygons) with snapping and keyboard nudging.
* Computes per‑ROI **time spent**, **distance traveled**, **speed**, and **detection/missing data** summaries.
* Exports **plots** (PNG/PDF) and **CSV** tables (binned results + frame‑by‑frame ROI occupancy).

---

##Requirements

### System

* **R ≥ 4.2**
* **FFmpeg** (ffmpeg & ffprobe) — used to extract a frame and FPS from the video
* **ImageMagick** — used by the `magick` package to read/display frames

**Default paths in the app:**

```r
ffmpeg:  "/opt/homebrew/bin/ffmpeg"
ffprobe: "/opt/homebrew/bin/ffprobe"
```

> If your FFmpeg binaries live elsewhere (Windows/Linux), either put them on your PATH or edit the two lines in `extract_frame()` and `extract_fps()`.

### R packages

Install once:

```r
install.packages(c(
  "shiny","bslib","shinyWidgets","hdf5r","magick","dplyr","ggplot2",
  "MASS","sf","shinyjs","sp","tidyr","viridis","purrr","stringr","zoo"
))
```

---

## 📦 Getting started

1. **Save the app** (this repository’s `app.R` / script content) into a folder, e.g. `PACERTrace/`.
2. **Open R or RStudio** in that folder.
3. **Run the app:**

   ```r
   shiny::runApp(".")
   ```

> Tip: You can also launch with `Rscript -e "shiny::runApp('.')"` from a terminal.

---

##️ Workflow overview

### 1) Video → Extract a frame

* Upload the **MP4** video in **Video Setup**.
* Choose a timestamp (seconds) and click **Extract Frame**.

  * This saves a PNG snapshot and records image dimensions.
  * The app also reads **FPS** via `ffprobe` for time conversion.

### 2) Calibrate (px → cm)

* Click **Start line**, then click **two points** on something with a known real length (e.g., chamber arm).
* Enter the real length and units, click **Apply**. You’ll see **px/cm** in the blue panel.

  * Calibration enables real‑world units for distance/speed and shape placement in centimeters.

### 3) Build ROIs

* Load existing ROIs (`.csv`) or create new ones:

  * **Place Shape**: Box/Rect/Circle with given dimensions (in **cm**) and rotation.

    * Requires calibration (px/cm) to place to scale.
    * Click once on the frame to drop the shape; give it a name and **Save Current Poly**.
  * **Draw Polygon**: Freehand polygon; click to add points; name it and **Save Current Poly**.
* **Snapping** helps align vertices/edges across ROIs; adjust snap distance (px) or disable.
* **Move/edit:**

  * Click **inside a ROI** to select it.
  * **Arrow keys** nudge the selected ROI (hold **Shift** for bigger steps).
  * Click near a vertex to select it, then **arrow keys** move just that vertex.
  * **Center on target** aligns the selected ROI centroid to another ROI.
* **Keyboard shortcuts:**

  * **Ctrl/Cmd + S** → Save current polygon
  * **Ctrl/Cmd + Z** → Undo last polygon point
  * **Esc** → Deselect
  * **Arrow keys** → Move ROI / vertex (hold **Shift** = faster)

### 4) Inspect SLEAP `.h5`

* Upload your SLEAP **analysis HDF5** file (`.h5`) and click **🔍 Inspect H5 Structure**.

  * PACERTrace expects datasets **`tracks`** and **`node_names`**.
  * The inspector reports array shape (3D/4D), frames, nodes, and basic data sanity.

### 5) Run analysis

* Set **Bin size (s)** for summaries and **Interpolate gaps ≤ (frames)** for short dropouts.
* Click **▶️ Run Analysis**.
* Use **Data Visualization** controls to choose:

  * **Trajectory** (facet by time bins or a single bin; optional clipping to selected ROIs)
  * **Position heatmap** / **Temporal heatmap** (per time chunk)
  * **Outside ROI heatmap** (where detected points are outside all ROIs)
  * **Detection raster** (detected vs missing over time)
  * **Missing data %** (per body part)
  * **ROI time spent** (sum over bins)
  * **Cumulative distance** / **Distance per bin**
* Overlay selected ROIs (dashed) on spatial plots.

### 6) Export

* **Plots:** **Export PNG** or **Export PDF** (wider pages for faceted heatmaps/trajectories).
* **Tables:**

  * **⬇️ CSV**: Binned analysis across ROIs and nodes
  * **⬇️ Frame Data CSV**: Per‑frame coordinates + boolean ROI occupancy (one column per ROI)
* **ROI definitions:** **Download ROI Definitions** (CSV with calibration metadata embedded).

---

## File formats

### ROI CSV (produced by PACERTrace)

Each row is a vertex of a named polygon. Columns include:

* `Name`, `x`, `y`
* `FrameWidth`, `FrameHeight` (of the reference frame when drawn)
* `VideoFile`, `VideoPath`, `FrameTime_sec`, `CreatedDate`, `CreatedTime`
* Calibration metadata (repeated on each row so it’s self‑contained):

  * `Calib_PxPerCm`, `Calib_LineLength_px`, `Calib_RealLength_cm`, `Calib_Units`
  * `Calib_Point1_X`, `Calib_Point1_Y`, `Calib_Point2_X`, `Calib_Point2_Y`

When you **load** an ROI CSV:

* If `FrameWidth/FrameHeight` differ from the current frame, PACERTrace **auto‑scales** the ROI vertices.
* If calibration columns are present, px/cm and the calibration line are **restored**.

### Binned analysis CSV

Per **Bin** and **ROI** (and **Node** if multi‑part), includes:

* `Total_Frames`, `Valid_Frames`
* `Frames` (frames inside ROI), `Time_sec` (inside ROI)
* `Bin_Start`, `Bin_End`
* Distance metrics per bin: `Distance_px`, `Distance_cm`, `MeanSpeed_cm_s`
* Metadata: `VideoFile`, `VideoFPS`, `ImageWidth/Height`, `PxPerCm`, calibration fields, export timestamp

### Frame‑by‑frame CSV

For each **frame × node**:

* `Frame`, `Time_sec`, `Node`, `X_coord`, `Y_coord`, `Valid`
* Steps/speed/cumulative distance (`*_px`, `*_cm` if calibrated)
* One boolean column per ROI: `ROI_<Name>`
* Metadata: video/calibration/export info

---

## 📊 Notes on SLEAP `.h5`

* `tracks` can be **3D** `[frames, nodes, coordinates]` or **4D** `[frames, nodes, coordinates, instances]`.
* `node_names` must align with the `nodes` dimension in `tracks`.
* The app merges instances per node by selecting the first valid (non‑NA, finite) coordinate per frame.

---

## 🧪 Calibration & units

* Without calibration, distance/speed are in **pixels**.
* With calibration, ROI placement for shapes (Box/Rect/Circle) uses **cm** dimensions and plots report **cm** and **cm/s**.
* You can re‑apply calibration at any time; exports capture the current px/cm.

---

## 🐞 Troubleshooting

* **FFmpeg not found / FPS failed**

  * Install FFmpeg; ensure `ffmpeg` and `ffprobe` are on your PATH or edit the absolute paths in the code.
* **ROI file loads but shapes appear off‑screen or tiny/huge**

  * Ensure your ROI CSV includes `FrameWidth/FrameHeight` so the app can auto‑scale. Otherwise, build ROIs against the exact same frame dimensions.
* **“No `tracks`/`node_names` dataset found”**

  * Verify your `.h5` is the **SLEAP analysis** file, not raw video or a different HDF5.
* **No movement metrics in cm**

  * Perform **Line Calibration** and click **Apply**; then rerun analysis.
* **ROIs not saving**

  * For polygons, you need **≥3 points** and a **non‑empty name** before **Save Current Poly**.

---

## ⌨️ Quick reference

* **Modes**: Select/Move · Place Shape · Draw Polygon
* **Nudge**: Arrow keys (hold Shift to move faster)
* **Save polygon**: Ctrl/Cmd + S
* **Undo point**: Ctrl/Cmd + Z
* **Deselect**: Esc
* **Select next vertex**: *Next vertex ▶* button

---

## 🧭 Repro tips

* Keep ROI names short and consistent (e.g., `ArmA`, `Center`, `RewardZone`).
* Export **frame‑by‑frame CSV** for downstream stats or quality control.
* Use **Detection raster** and **Missing data %** to distinguish tracking dropouts from animal leaving ROIs.

---

## 📚 Citation

If PACERTrace supports a figure or analysis in your work, please cite this repository and version: **PACERTrace v1.0.0**.

---

## 📄 License

MIT License

Copyright (c) 2025 <Ames Sutton Hickey>

Permission is hereby granted, free of charge, to any person obtaining a copy
