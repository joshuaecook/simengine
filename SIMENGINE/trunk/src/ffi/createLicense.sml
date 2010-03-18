
open License

val developer_license =
    make {product=SIMENGINE,
     customerID=0,
     customerName="Simatra Developer",
     customerOrganization="Simatra",
     restriction=SITE "Simatra Headquarters",
     serialNumber=0,
     maxMajorVersion=9999,
     maxMinorVersion=9999,
     expirationDate=NONE,
     version=DEVELOPMENT,
     enhancements={}}

val developerLicenseData = licenseToData(developer_license)

val _ = TextIO.output(TextIO.stdOut, developerLicenseData)
