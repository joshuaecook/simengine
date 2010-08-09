#include<stdio.h>
#include<string.h>
#include<time.h>

const int secondsPerDay = 60*60*24;
const char message[] = "Trial license for ";

int main(int argc, char **argv){
  unsigned char licenseData[256] = {0};
  unsigned char licenseKey[512] = {0};
  int *intLicenseData = (int*) licenseData;
  int trialID;
  char *name;
  char *email;
  int expDate;
  int offset;
  int keylen;
  int ret;

  // Make sure exactly the correct number of arguments are passed
  if(argc != 4)
    return 6;

  // Parse args
  trialID = atoi(argv[1]);
  if(trialID < 1)
    return 6;

  name = argv[2];
  email = argv[3];

  if(strlen(name) == 0 || strlen(email) == 0 || strlen(name) + strlen(email) > 75)
    return 6;

  // Create expiration
  expDate = time(0)/secondsPerDay + 30;

  // Create license data
  intLicenseData[0] = 0x00cafe00; // Header
  intLicenseData[1] = 0; // Product
  intLicenseData[2] = 0; // Serial Number
  intLicenseData[3] = 4; // Version (Trial)
  intLicenseData[4] = 9999; // Max major version
  intLicenseData[5] = 9999; // Max minor version
  intLicenseData[6] = trialID; // Customer ID
  intLicenseData[7] = 3; // Site license
  intLicenseData[8] = expDate; // Expiration
  intLicenseData[9] = 0; // Enhancements
  intLicenseData[10] = strlen(name); // Length of name field
  intLicenseData[11] = strlen(email); // Length of organization field
  intLicenseData[12] = strlen(name) + strlen(message) + 1; // Length of site license field
  offset = 13 * sizeof(int);
  sprintf(licenseData + offset, "%s", name); // Name
  offset += strlen(name);
  sprintf(licenseData + offset, "%s", email); // Organization (email for trial)
  offset += strlen(email);
  sprintf(licenseData + offset, "%s", message); // Site license message
  offset += strlen(message);
  sprintf(licenseData + offset, "%s", name);
  offset += strlen(name);
  sprintf(licenseData + offset, ".");
  offset += 1;

  ret = encode(offset, licenseData, &keylen, licenseKey);
  if(0 == ret)
    printf("%s", licenseKey);

  return ret;
}
