/*
    swbe_main.c

    This is the main file that constitutes a software backend simulation.  This file is included
    from a generated model and has the generic infrastructure code for communication to and from
    a model.

    Copyright 2009 Simatra Modeling Technologies
    Copyright 2009 Carl Lebsack
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <time.h>

// LLC commands and packet processing routines
#include <llc_defs.h>

// Network socket constants
#define SOCKET_ERROR 1
#define SOCKET_SUCCESS 0

#define NO_TAG (0)

// Debug flag for extra output on stderr
int debug = 0;

FILE *debuglog;

enum {CMD, DATA, NUM_PORTS};

typedef struct {
  int sock[NUM_PORTS];
  int port[NUM_PORTS];
} sockport;

// Forward declaration of generated software model
void run_sw_model(int cmdsock, int datasock, char *statfilename);

// This opens the network sockets by setting up servers to listen for connections
int open_sockets(sockport *sp){
  int sockfd[NUM_PORTS];

  socklen_t clilen;
  struct sockaddr_in serv_addr, cli_addr;
  int optval;
  int flags;
  int port;

  // Set CMD and DATA sockets to listen for connections
  for(port=CMD;port<NUM_PORTS; port++){
    // Create a socket descriptor
    sockfd[port] = socket(AF_INET, SOCK_STREAM, 0);
    if(sockfd[port] < 0){
      perror("socket: ");
      return SOCKET_ERROR;
    }
    // Make sure we can reuse the same ports for successive connections
    optval = 1;
    if(-1 == setsockopt(sockfd[port], SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval))){
      perror("setsockopt: ");
      return SOCKET_ERROR;
    }
    // Bind the socket descriptor to an address and port
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = INADDR_ANY;
    serv_addr.sin_port = htons(sp->port[port]);
    if (bind(sockfd[port], (struct sockaddr *) &serv_addr, sizeof(serv_addr)) < 0){
      perror("bind: ");
      return SOCKET_ERROR;
    }
    // Listen for an incomming connection
    listen(sockfd[port],1);
  }
  clilen = sizeof(cli_addr);

  // Wait until both connections are established
  for(port=CMD;port<NUM_PORTS; port++){
    if(sp->sock[port] == 0){
      // Accept an incoming connection
      sp->sock[port] = accept(sockfd[port], (struct sockaddr *) &cli_addr, &clilen);
      if(sp->sock[port] < 0){
	sp->sock[port] = 0;
	perror("accept: ");
	return SOCKET_ERROR;
      }
    }
    // Close listening socket after connection established
    shutdown(sockfd[port], SHUT_RDWR);
    close(sockfd[port]);

    // Set connected socket to non-blocking
    flags = fcntl(sp->sock[port], F_GETFL, NULL);
    flags |= O_NONBLOCK;
    if (fcntl(sp->sock[port], F_SETFL, flags) < 0) {
      perror("fcntl O_NONBLOCK: ");
      return SOCKET_ERROR;
    }
  }

  return SOCKET_SUCCESS;
}

// Closes the network sockets
void close_sockets(sockport *sp){
  // Close sockets and clear data structure
  shutdown(sp->sock[CMD], SHUT_RDWR);
  shutdown(sp->sock[DATA], SHUT_RDWR);
  close(sp->sock[CMD]);
  close(sp->sock[DATA]);
  bzero(sp, sizeof(sockport));
}

// Main routine, connects sockets and runs a model
int main(int argc, char **argv)
{
  sockport sp = {0};
  char *statfilename;

  if (argc < 3 || argc > 5){
    printf("Usage %s <cmd port num> <data port num> [statfile] [d]\n", argv[0]);
    return 1;
  }

  // Turn on debugging output
  if((argc == 4 && strcmp(argv[3], "d") == 0) ||
     (argc == 5 && strcmp(argv[4], "d") == 0))
    debug = 1;

  // Set up statfile, used only for debug backend
  if(debug && argc == 4 || argc == 3)
    statfilename = "statfile";
  else
    statfilename = argv[3];
  
  if(debug)
    debuglog = fopen("debuglog", "w");

  // Parse port numbers from command line
  sp.port[CMD] = atoi(argv[1]);
  sp.port[DATA] = atoi(argv[2]);

  // Initialize random number generator, debug backend
  srand(time(NULL));
  
  // Open sockets and wait for connection
  open_sockets(&sp);

  // Run the software model
  run_sw_model(sp.sock[CMD], sp.sock[DATA], statfilename);

  // Close sockets
  close_sockets(&sp);

  if(debug)
    fclose(debuglog);
  
  return 0;
}


// GENERIC CMD PROCESSING
// Forward declaration of generated routines for cmd_process()
unsigned int get_status();
unsigned int get_sample_count();
float get_output(unsigned int tag);

// Process any pending commands and return requested information
int cmd_process(int cmdsock){
  unsigned long packet;
  int haspayload;
  union { float f; unsigned long ul; } payload;
  int sz;
  
  do{
    // Read a command if available
    sz = read(cmdsock, &packet, sizeof(packet));
       
    // A command was received
    if(sizeof(packet) == sz){
      packet = ntohl(packet);
      unsigned char command = getcmd(packet);
      unsigned char datalen = getdatalen(packet);
      unsigned short tag = gettag(packet);

      if(debug){
	fprintf(debuglog, "cmd %08x\n", packet);
	fprintf(debuglog, "\tcommand = %d\n", command);
	fprintf(debuglog, "\ttag = %d\n", tag);
	fprintf(debuglog, "\tdatalen = %d\n", datalen);
	fflush(debuglog);
      }
      
      // Use same packet and payload for response
      packet = 0;
      haspayload = 0;
      payload.ul = 0;
       
      // Set up response to commands
      switch(command){
      case HOST_CMD_NIL:
	break;
       
      case HOST_CMD_GET_STATUS:
	packet = makeheader(MODEL_CMD_SEND_STATUS, 1, NO_TAG);
	payload.ul = get_status();
	haspayload = 1;
	break;
       
      case HOST_CMD_GET_SAMPLE_COUNT:
	packet = makeheader(MODEL_CMD_SEND_SAMPLE_COUNT, 1, NO_TAG);
	payload.ul = get_sample_count();
	haspayload = 1;
	break;
       
      case HOST_CMD_GET_HASH:
	packet = makeheader(MODEL_CMD_SEND_HASH, 1, NO_TAG);
	payload.ul = 0xdec0de; // SW doesn't have a real hashcode, maybe it should
	haspayload = 1;
	break;
       
      case HOST_CMD_GET_ITERATOR:
	packet = makeheader(MODEL_CMD_SEND_ITERATOR, 1, NO_TAG);
	payload.f = get_output(0); // Iterator should always be tag 0
	haspayload = 1;
	break;
       
      case HOST_CMD_GET_OUTPUT:
	packet = makeheader(MODEL_CMD_SEND_OUTPUT, 1, tag);
	payload.f = get_output(tag);
	haspayload = 1;
	break;
       
      default:
	return SOCKET_ERROR;
      };

      if (debug) {
	  fprintf(debuglog, "packet = %08x\n", packet);
	  fflush(debuglog);
	}

      // Send response
      if(packet){
	packet = htonl(packet);	
	if(sizeof(packet) != write(cmdsock, &packet, sizeof(packet))){
	  perror("write to socket: ");
	  return SOCKET_ERROR;
	}
      }
      if(debug) {
	fprintf(debuglog, "  payload=%08x\n", payload.ul);
	fflush(debuglog);
      }
      if(haspayload){
	payload.ul = htonl(payload.ul);
	if(sizeof(payload.ul) != write(cmdsock, &payload.ul, sizeof(payload.ul))){
	  perror("write to socket: ");
	  return SOCKET_ERROR;
	}
      }
    }
    else if(-1 == sz){
      // Check for error other than no data available
      if(errno != EWOULDBLOCK)
	return SOCKET_ERROR;
    }
    // Got a bogus return on read call, not a multiple of 4
    else{
      return SOCKET_ERROR;
    }
  }while(sz > 0);  // Process all pending commands
       
  return SOCKET_SUCCESS;
}
       
// GENERIC DATA PROCESSING
// Forward declaration of generated routines for data_process()
void run_for(int datasock, float val);
void model_reset();
void output_enable(unsigned int tag, unsigned int en);
void set_param(unsigned int tag, float val);

int data_process(int datasock){
  unsigned long packet;
  union { float f; unsigned long ul; } payload;
  int sz;
  
  do{
    // Read a command if available
    sz = read(datasock, &packet, sizeof(packet));
       
    // A command was received
    if(sizeof(packet) == sz){
      packet = ntohl(packet);
      unsigned char command = getcmd(packet);
      unsigned char datalen = getdatalen(packet);
      unsigned short tag = gettag(packet);
      
      if(debug){
	fprintf(debuglog, "data %08x\n", packet);
	fprintf(debuglog, "\tcommand = %d\n", command);
	fprintf(debuglog, "\ttag = %d\n", tag);
	fprintf(debuglog, "\tdatalen = %d\n", datalen);
	fflush(debuglog);
      }

      // Handling of specific commands
      switch(command){
      case HOST_DATA_NIL:
	break;
	
      case HOST_DATA_RUN:
	
	do{
	  sz = read(datasock, &payload.f, sizeof(payload.f));
	} while(sz == -1 && errno == EWOULDBLOCK);
	if(sz != sizeof(payload.f)){
	  perror("read socket: ");
	  return SOCKET_ERROR;
	}
	
	payload.ul = ntohl(payload.ul);

	if (debug){
	  fprintf(debuglog, "  run for %f received\n", payload.f);
	  fflush(debuglog);
	}

	run_for(datasock, payload.f);
	unsigned long data_begin = htonl(makeheader(MODEL_DATA_BEGIN_DATA, 0, 0));
	int bytessent=0, totalbytestosend = sizeof(data_begin);
  
	do {
	  if (debug){
	    fprintf(debuglog, "passing through sending begin data loop with bytessent=%d and totalbytestosend=%d\n", bytessent, totalbytestosend);
	    fflush(debuglog);
	  }
	  bytessent = send(datasock, &data_begin, totalbytestosend, 0);
	  if (bytessent != -1)
	    totalbytestosend -= bytessent;
	}
	while(totalbytestosend > 0 || (bytessent == -1 && errno != EAGAIN));

	// Return because no other data commands should be processed
	// until iterator time is reached
	return SOCKET_SUCCESS;
	
      case HOST_DATA_RESET:
	model_reset();
	break;

      case HOST_DATA_ENABLE_OUTPUT:
	if (debug){
	  fprintf(debuglog, "\tHOST_DATA_ENABLE_OUTPUT receieved for tag=%d\n", tag);
	  fflush(debuglog);
	}
	output_enable(tag, 1);
	break;

      case HOST_DATA_DISABLE_OUTPUT:
	if (debug){
	  fprintf(debuglog, "\tHOST_DATA_DISABLE_OUTPUT receieved for tag=%d\n", tag);
	  fflush(debuglog);
	}
	output_enable(tag, 0);
	break;

      case HOST_DATA_SET_PARAM:
	if (debug){
	  fprintf(debuglog, "\tHOST_DATA_SET_PARAM receieved\n");
	  fflush(debuglog);
	}
	do{
	  sz = read(datasock, &payload.f, sizeof(payload.f));
	} while (sz == -1 && errno == EWOULDBLOCK);
	if(sz != sizeof(payload.f)){
	  perror("read socket: ");
	  return SOCKET_ERROR;
	}
	payload.ul = ntohl(payload.ul);
	if (debug){
	  fprintf(debuglog, "\t\tsetting %d to %f\n",tag, payload.f);
	  fflush(debuglog);
	}
	set_param(tag, payload.f);
	break;

      default:
	return SOCKET_ERROR;
      };
    }
    else if(-1 == sz){
      if(errno != EWOULDBLOCK)
	return SOCKET_ERROR;
    }
    else{
      return SOCKET_ERROR;
    }
  }while(sz > 0);

  return SOCKET_SUCCESS;
}


void data_send(int datasock, char *buffer, int sample_size) {
  if (sample_size > 0) {
    int bytes_sent = 0;

    if(debug){
      fprintf(debuglog, "in data_send with buffer of len %d to write\n", sample_size);
      fflush(debuglog);
    }

    do {
      // first send the data
      if ((bytes_sent = send(datasock, buffer, sample_size, 0)) == -1 && errno != EAGAIN){
        perror("send");
	return;
      }
      
      // keep track of the total bytes sent
      if (bytes_sent != -1){
	sample_size -= bytes_sent;
	buffer += bytes_sent;
	if(debug){
	  fprintf(debuglog, "in LOOP in data_send with buffer of len %d to write\n", sample_size);
	  fflush(debuglog);
	}
      }
      
    } while (sample_size > 0);
  }
}
