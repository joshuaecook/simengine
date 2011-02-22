#include<iostream>
#include<fstream>
#include"DataStore.pb.h"

using namespace std;

void ListValues(const DataGroup& dg) {
  int i,j;

  for (i = 0; i < dg.member_size(); i++) {
    const DataValue& dv = dg.member(i);

    cout << dg.name() << "{" << i << "}" << "[" << dv.dims(0);

    for(j=1;j<dv.dims_size();j++){
      cout << ", " << dv.dims(j);
    }

    cout << "] :\n";

    for(j=0;j<dv.dreal_size();j++){
      cout << "\t" << dv.dreal(j);
      if(dv.dimag_size()){
	cout << " + i*" << dv.dimag(j);
      }
      cout << "\n";
    }

    for(j=0;j<dv.sreal_size();j++){
      cout << "\t" << dv.sreal(j) << "f";
      if(dv.simag_size()){
	cout << " + i*" << dv.simag(j) << "f";
      }
      cout << "\n";
    }

    cout << "\n";
  }
}

void ListGroups(const DataStore& ds){
  int i;
  for(i=0;i<ds.group_size();i++){
    const DataGroup& dg = ds.group(i);
    ListValues(dg);
  }
}

int main(int argc, char **argv){
  // Verify that the version of the library that we linked against is
  // compatible with the version of the headers we compiled against.
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  DataStore ds;
  fstream input;

  if (argc != 2) {
    cerr << "Usage:  " << argv[0] << " DATA_STORE_FILE" << endl;
    return -1;
  }

  {
    // Read the existing address book.
    input.open(argv[1], ios::in | ios::binary);
    if (!ds.ParseFromIstream(&input)) {
      cerr << "Failed to parse address book." << endl;
      return -1;
    }
    input.close();
  }

  ListGroups(ds);

  // Optional:  Delete all global objects allocated by libprotobuf.
  google::protobuf::ShutdownProtobufLibrary();

  return 0;
}
