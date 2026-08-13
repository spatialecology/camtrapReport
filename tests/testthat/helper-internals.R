# Internal-function accessor used only by the test suite.
#
# This avoids using the ::: operator repeatedly while retaining
# access to non-exported package functions for unit tests.

ct_internal <- function(name) {
  getFromNamespace(
    name,
    "camtrapReport"
  )
}
