# Changelog

All notable changes to PACERTrace will be documented in this file.

## [1.0.1] - 2025-01-10

### Fixed
- Plot export now correctly saves as PNG files instead of HTML
- Export files now save to user's designated download folder
- Added error handling for failed plot exports
- Fixed "argument is of length zero" errors in observe blocks

### Added
- Multiple ROI overlay support on trajectory plots
- Calibration data saved with ROI definitions
- Enhanced debug tools for coordinate alignment
- Keyboard shortcuts reference panel
- Modern UI theme using bslib

### Changed
- Improved trajectory plot alignment with image coordinates
- Better error handling throughout the app
- Split validation checks to prevent length-zero errors

## [1.0.0] - 2025-01-08

### Initial Release
- ROI drawing and management with multiple shapes (box, rectangle, circle, polygon)
- SLEAP H5 file analysis support (3D and 4D structures)
- Multiple plot types (trajectory, heatmaps, rasters, distance, speed)
- Frame-by-frame data export with full metadata
- Line calibration for distance measurements
- Outside-ROI detection and analysis
- Video frame extraction with FFmpeg
- Keyboard shortcuts for efficient workflow
