# install.ps1
param(
    [string]$Destination = "$HOME\.local\bin"
)

$ErrorActionPreference = "Stop"
Push-Location $PSScriptRoot

try {
    Write-Host "Building..."
    dune build

    # Create destination directory if it doesn't exist
    New-Item -ItemType Directory -Force -Path $Destination | Out-Null

    $targetFile = Join-Path $Destination "henle.exe"
    
    # remove existing binary first to avoid permission issues
    if (Test-Path $targetFile) {
        try {
            Remove-Item $targetFile -Force
        } catch {
            Write-Host "Couldn't remove the existing $targetFile." -ForegroundColor Red
            Write-Host "It may be in use or require administrator privileges."
            Write-Host "If it's in use, close any running instances of henle and try again."
            exit 1
        }
    }

    # copy newly built binary
    Copy-Item "_build\default\bin\main.exe" $targetFile
    Write-Host "Installed to $targetFile" -ForegroundColor Green

    # check if the destination is in the system PATH
    $pathDirs = $env:PATH -split ";"
    if (-not ($pathDirs -contains $Destination)) {
        Write-Host "Note: $Destination doesn't appear to be on your PATH." -ForegroundColor Yellow
        Write-Host "Add it to your Windows Environment Variables to use 'henle' from anywhere."
    }
} finally {
    Pop-Location
}