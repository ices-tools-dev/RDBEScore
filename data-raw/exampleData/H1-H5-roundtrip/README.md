These ZIPs were downloaded from production RDBES on 2026-10-15 after the
original H1/H5 examples were uploaded and screened. They contain additional
records returned by the country/year/scheme filters. Preserve these raw exports.

Run `data-raw/create_H1_ExampleData.R` and `data-raw/create_H5_ExampleData.R`
from the project root. They compare against the original 2025-02-11 ZIPs,
select the original observations, and restore the original primary and foreign
IDs together. The preparation rejects unexplained changed or missing records.
It regenerates packaged data and the dated test fixtures without live API calls
when the saved download exists.

The upload changed `National Routine` to the registered `WGRDBES-EST TEST 1`
scheme and converted dates to ISO format. H5 CL/CE use the current vocabulary:
`Estimate` to `Estimated`, `Combcd` to `CombOD`, `AdjPosData` to `PosDat`,
marine missing freshwater names to the literal code `NA`, and `NoDiff` to
`NoDiffKnown` where the supplied official and scientific weights are equal.
Numeric observations are retained.

Accepted by rix133 on 2026-10-15: retain corrected H1 species-list links.
Five legacy SL IDs pointed to differently named lists than SS requested.
The prepared data retain the old numeric IDs, with the SL metadata and IS
membership of the lists named by SS and linked by the server. Sampling
observations, including all BV measurements, are unchanged.

H1 has no CL/CE in its source ZIP. Its flat CSV test fixture additionally retains
the original local CL/CE and malformed-vessel fixture for the existing tests.
These local tables are excluded from the packaged H1 object and its ZIP tests.
