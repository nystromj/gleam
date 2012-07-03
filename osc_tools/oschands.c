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
 *
 */ 

//-------------+---------------------------------------------------------------
//   Headers   |
//-------------+

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "lo/lo.h"

//-------------+---------------------------------------------------------------
//   Globals   |                                                              
//-------------+

int done = 0;

//---------------+-------------------------------------------------------------
//   Functions   |                                                            
//---------------+ 

void error(int num, const char *m, const char *path);

int valid_port (int port);

int joint_handler(const char *path, const char *types, lo_arg **argv, int argc,
		void *data, void *user_data);

int quit_handler(const char *path, const char *types, lo_arg **argv, int argc,
		 void *data, void *user_data);

//------------+----------------------------------------------------------------
//    Main    |
//------------+

int main (int argc, char * argv[])
{
  /* check for read  port and write port */
  if (argc < 3)
    {
      printf("- Missing Argument -\nUsage: oschand read_port write_port\n");
      exit(1);
    } // if

  if (!valid_port (argv[1]))
    {
      printf (" -Invalid Read Port- ");
      exit (argv[1]);
    }

  if (!valid_port (argv[2]))
    {
      printf (" -Invalid Write Port -");
      exit (argv[2]);
    }

  /* if ports are okay, start a new server with error handler */
  lo_server_thread st = lo_server_thread_new(read_port, error);
  lo_address addr = lo_address_new("127.0.0.1", write_port);

  /* add method that will handle joints */
  lo_server_thread_add_method(st, "/joint", "sifff", joint_handler, addr);

  /* add method that will match the path /quit with no args */
  lo_server_thread_add_method(st, "/quit", "", quit_handler, NULL);

  lo_server_thread_start(st);

  while(!done)
    {
#ifdef WIN32
      Sleep(1);
#else
      usleep(1000);
#endif
    } // while

  lo_server_thread_free(st);
  lo_address_free(addr);

  return 0;
} // main

//---------------+------------------------------------------------------------
//   Handlers    |                                                          
//---------------+ 

/* error handler */
void error(int num, const char *msg, const char *path)
{
  printf("liblo server error %d in path %s: %s\n", num, path, msg);
} // error

/* stub for validating ports... to be implemented later */
void valid_port (int port)
{
  return 1;
} // valid_port
 
/* catch joint messages and send only l_hand and r_hand messages */
int joint_handler(const char *path, const char *types, lo_arg **argvx, 
		    int argc, void *data, void *user_data)
{

  // if no message, return 1
  if(!data)
    return 1;

  // if left or right hand, send message to write_port
  if (strcmp(&argvx[0]->s, "l_hand") == 0 ||
      strcmp(&argvx[0]->s, "r_hand") == 0)
    {
      lo_send(user_data, path, types, &argvx[0]->s, argvx[1]->i, 
	      argvx[2]->f, argvx[3]->f, argvx[4]->f);
    } // if

  return 1;
} // joint_handler

/* handler for quit messages */
int quit_handler(const char *path, const char *types, lo_arg **argv, int argc,
		 void *data, void *user_data)
{
  done = 1;
  printf("quitting\n\n");

  return 0;
} // quit_handler

