# msaidPlatformR

An R SDK for the **MSAID Platform** for mass spectrometry data analysis. This package provides R users with programmatic access to experiment data through a clean, R-native interface that handles authentication, data retrieval, caching, and data processing workflows.

## Platform

Access the MSAID Platform at: https://platform.msaid.io

## Installation

Install the package from GitHub using devtools:

```r
devtools::install_github("msaid-de/msaidPlatformR")
```

## Quick Start

```r
library(msaidPlatformR)

# Login to the platform
platform_login()

# List available experiments
experiments <- platform_list_experiments()

# Read experiment data
data <- platform_read_experiment_results(
  level = "psms",
  experiment_names = "your_experiment",
  max_global_q_value = 0.01
)
```

## License

This package is licensed under the Apache License 2.0.

## Support

For support, please contact [support@msaid.de](mailto:support@msaid.de) or visit [msaid.de](https://msaid.de).

## Copyright

© 2025 MSAID GmbH. All rights reserved.
