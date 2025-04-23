# Build and test script for AdaVisionUnit
# Requires GNAT with gprbuild and GNATCOLL.JSON installed and on PATH
# Usage: Open PowerShell in project root and run `.uild_and_test.ps1`

# Build the project
Write-Host "Building AdaVisionUnit project..."
gprbuild -P ada_vision_unit.gpr
if ($LASTEXITCODE -ne 0) {
    Write-Error "Build failed with exit code $LASTEXITCODE"; exit $LASTEXITCODE
}

# Run unit tests
Write-Host "Running unit tests..."
& .\bin\ada_vision_unit_test_runner
if ($LASTEXITCODE -ne 0) {
    Write-Error "Unit tests failed with exit code $LASTEXITCODE"; exit $LASTEXITCODE
}

# Example CLI usage: dataset statistics (replace <labels_dir> as needed)
Write-Host "Running CLI stats example..."
& .\bin\ada_vision_unit_cli stats .\src
if ($LASTEXITCODE -ne 0) {
    Write-Error "CLI stats command failed with exit code $LASTEXITCODE"; exit $LASTEXITCODE
}

Write-Host "Build and tests completed successfully."