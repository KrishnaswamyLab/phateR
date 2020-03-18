---
name: Bug report
about: Create a report to help us improve
title: ''
labels: bug
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Standalone code to reproduce the error

**Expected behavior**
A clear and concise description of what you expected to happen.

**Actual behavior**
Please include the full traceback of any errors

**System information:**

Output of `phate.__version__`:

```
Please run phate.__version__ and paste the results here.

You can do this with `python -c 'import phate; print(phate.__version__)'`
```

Output of `pd.show_versions()`:

<details>

```
Please run pd.show_versions() and paste the results here.

You can do this with `python -c 'import pandas as pd; pd.show_versions()'`
```

</details>

Output of `sessionInfo()`:

<details>

```
Please run sessionInfo() and paste the results here.

You can do this with `R -e 'library(phateR); sessionInfo()'`
```

</details>

Output of `reticulate::py_discover_config()`:

<details>

```
If you are running MAGIC in R, please run `reticulate::py_discover_config(required_module = "phate")` and paste the results here.

You can do this with `R -e 'reticulate::py_discover_config(required_module = "phate")'`
```

</details>

Output of `phateR::check_pyphate_version()`:

<details>

```
Please run `phateR::check_pyphate_version()` and paste the results here.

You can do this with `R -e 'phateR::check_pyphate_version()'`
```

</details>

**Additional context**
Add any other context about the problem here.
