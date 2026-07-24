platform <- tolower(Sys.info()[["sysname"]])
platform <- switch(
  platform,
  darwin = "macos",
  windows = "windows",
  linux = "linux",
  platform
)
cat(sprintf("platform=%s\narch=%s\n", platform, Sys.info()[["machine"]]))
