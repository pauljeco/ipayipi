% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{sts_phens}
\alias{sts_phens}
\title{Standardised phenomena name and metadata table}
\format{
\subsection{'sts_phens'}{

A data.table with 15 columns:
\describe{
\item{phid}{The unique phenomena identifier}
\item{phen_name_full}{A description of the phenomena. Follow convention of 'phen_type' : description of measure/units/extras}
\item{phen_type}{The type of phenomena being measured. Add new values to the 'phen_type_tbl_sts' sheet.}
\item{phen_name}{"The 'column' or 'field' name used in ipayipi. Duplicate 'phen_name' are not allowed in this worksheet.
The measure is include as a suffix to 'phen_name' except 1) where the measure is a sample ('smp'), and/or 2) NB! the suffix '_sn' denotes a serial number or identifier."}
\item{units}{Units standardised for ipayipi. Add new values to the 'var_type_tbl_sts' sheet.}
\item{measure}{Measure standardised for ipayipi. Add new values to the 'measure_type_tbl_sts' sheet.}
\item{offset}{A numeric additive that is applied to raw data during standardisation, e.g., 'phenomena_sts()'.  Add new values to the 'var_type_tbl_sts' sheet.}
\item{var_type}{The phenomena variable type standardised for ipayipi.}
\item{uz_phen_name}{The 'column' or 'field' name used in the raw data which is 'unstandardised' ('_uz') for ipayipi.}
\item{uz_units}{The units used in the raw data which is 'unstandardised' ('_uz') for ipayipi.}
\item{uz_measure}{The measure used in the raw data which is 'unstandardised' ('_uz') for ipayipi.}
\item{f_convert}{A conversion factor. Applies as the product (multiplier) of the 'raw' data. Can be used to transform units allowing the standardised data units to differ from 'raw' data. The conversion factor is applied by the 'phenomena_sts()' ipayipi function.}
\item{sensor_id}{The phenomena's unique identifier.}
\item{notes}{Additional notes, user input.}
}
}
}
\source{
Internal.
}
\usage{
sts_phens
}
\description{
Descriptions with metadata of phenomena standardised as per the SAEON
iPayipi format.
}
\examples{
# print phenomena standards table
ipayipi::sts_phens

}
\keyword{datasets}
