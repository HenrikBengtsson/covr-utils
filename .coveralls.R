#################################################################
# Test coverage
#
# * covr: https://github.com/jimhester/covr
# * Coveralls: https://coveralls.io/
#
# Henrik Bengtsson
#################################################################
## Load/install 'covr'
use_covr()

# Exclusion rules
excl <- exclusions(
  filter(r_files(), covr_lines), # Apply 'covr:' rules in source code
  filter(r_files(), stop_lines)  # Skip lines with stop().
)
str(excl)

# Run through tests, record source code coverage, and
# publish to Coveralls
covr_package(exclusions=excl)
