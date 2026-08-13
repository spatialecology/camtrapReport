# Helper for testing internal camtrapReport functions.
# Keeps internal functions internal while avoiding ::: in tests.

ct_internal <- function(name) {
  getFromNamespace(name, "camtrapReport")
}
