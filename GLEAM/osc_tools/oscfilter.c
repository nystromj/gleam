/**
 *
 *  Copyright (c) 2012 Jennelle Nystrom
 *
 *  Based on code by Steve Harris and Uwe Koloska for Sensebloom/osceleton 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation; either version 2.1 of the
 *  License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 */ 

//-------------+---------------------------------------------------------------
//   Headers   |
//-------------+

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <time.h>

#include "lo/lo.h"

//-------------+---------------------------------------------------------------
//   Globals   |                                                              
//-------------+

int done = 0;

int FILTER_SIGNATURE = 0;

/* filter struct to send data to message handlers*/ 
struct filter {
  int signature;     // a unique filter ID
  lo_address addr;   // the address to send messages to
  int num_joints;    // the number of joints in the joints array
  char ** joints;  // array of osc joint strings
};

//---------------+-------------------------------------------------------------
//   Functions   |                                                            
//---------------+ 

void error (int num, const char * msg, const char * path);

int joint_handler (const char *path, const char *types, lo_arg **argv, 
		   int argc, void *data, void *user_data);

int quit_handler (const char *path, const char *types, lo_arg **argv, 
		  int argc, void *data, void *user_data);

//------------+----------------------------------------------------------------
//    Main    |
//------------+

int main (int argc, char * argv[]) {
  lo_server_thread st;        // server to receive messages from
  lo_address addr;            // address to send messages to
  static struct filter ftr;   // stores data to be passed to handler
  int i, j;                   // index variables

  /* Set up the filter signature.  The signature helps us verify that things 
   * that we think are filters are actually filters. */
  srandom ((unsigned int) time (NULL));
  FILTER_SIGNATURE = random ();

  /* check for read port and write port */
  if (argc < 3)
    {
      printf ("- Missing Argument -\nUsage: oschand read_port write_port\n");
      exit (1);
    } // if

  /* start a new server and set up write port */
  st = lo_server_thread_new (argv[1], &error);
  addr = lo_address_new ("127.0.0.1", argv[2]);

  /* setup filter structure to be passed to joint handler */
  ftr.signature = FILTER_SIGNATURE;
  ftr.addr = addr;
  ftr.num_joints = argc - 3;
  ftr.joints = (char **) malloc (sizeof (char *) * ftr.num_joints);

  for (j = 0, i = 3; j < ftr.num_joints; j++, i++)
    ftr.joints[j] = argv[i];

  /* add method that will handle joints */
  lo_server_thread_add_method (st, "/joint", "sifff", joint_handler, &ftr);

  /* add method that will match the path /quit with no args */
  lo_server_thread_add_method (st, "/quit", "", quit_handler, NULL);

  lo_server_thread_start (st);

  /* wait for messages */
  while (!done)
    {
#ifdef WIN32
      Sleep (1);
#else
      usleep (1000);
#endif
    } // while

  lo_server_thread_free (st);
  lo_address_free (addr);

  return 0;
} // main

//---------------+------------------------------------------------------------
//   Handlers    |                                                          
//---------------+ 

/* error handler */
void error (int num, const char *msg, const char *path) {
  printf ("liblo server error %d: %s\n", num, msg);
} // error
 
/* catch joint messages and send only l_hand and r_hand messages */
int joint_handler (const char *path, const char *types, lo_arg **argvx, 
		   int argc, void *data, void * user_data) {
  int j;                  // counter variable
  struct filter *fltp;    // pointer to a struct filter

  /* if no message, return 1 */
  if (!data)
    return 1;

  /* verify that the filter exists. */
  if (user_data == NULL)
    return 1;

  /* convert user_data type */
  fltp = (struct filter *) user_data;

  /* verify that the user_data represents a filter */
  if (fltp->signature != FILTER_SIGNATURE)
    return 1;

  /* if the joint in the message matches a joint in filter, forward message */
  for (j = 0; j < fltp->num_joints; j++) {
    if (strcmp (&argvx[0]->s, fltp->joints[j]) == 0) 
      lo_send (fltp->addr, path, types, &argvx[0]->s, argvx[1]->i, 
	      argvx[2]->f, argvx[3]->f, argvx[4]->f);
  } // for

  return 1;
} // joint_handler

/* handler for quit messages */
int quit_handler (const char *path, const char *types, lo_arg **argv, int argc,
		  void *data, void *user_data) {
  done = 1;
  printf ("quitting\n\n");

  return 0;
} // quit_handler

