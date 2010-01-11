/* 
 * This file is part of OpenModelica.
 * 
 * It implements the Relational Meta-Language (RML) and MetaModelica
 * http://www.ida.liu.se/~pelab/rml
 * 
 * Copyright (c) 1998-2008, Linköpings University,
 * Department of Computer and Information Science, 
 * SE-58183 Linköping, Sweden. 
 * 
 * All rights reserved.
 * 
 * THIS PROGRAM IS PROVIDED UNDER THE TERMS OF THIS OSMC PUBLIC 
 * LICENSE (OSMC-PL). ANY USE, REPRODUCTION OR DISTRIBUTION OF 
 * THIS PROGRAM CONSTITUTES RECIPIENT'S ACCEPTANCE OF THE OSMC 
 * PUBLIC LICENSE. 
 * 
 * The OpenModelica software and the Open Source Modelica 
 * Consortium (OSMC) Public License (OSMC-PL) are obtained 
 * from Linköpings University, either from the above address, 
 * from the URL: http://www.ida.liu.se/projects/OpenModelica
 * and in the OpenModelica distribution.
 * 
 * This program is distributed  WITHOUT ANY WARRANTY; without 
 * even the implied warranty of  MERCHANTABILITY or FITNESS 
 * FOR A PARTICULAR PURPOSE, EXCEPT AS EXPRESSLY SET FORTH 
 * IN THE BY RECIPIENT SELECTED SUBSIDIARY LICENSE CONDITIONS 
 * OF OSMC-PL. 
 * 
 * See the full OSMC Public License conditions for more details.
 * 
 * @author Adrian Pop [adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo]
 * 
 */

/*
 *
 * [ socket.c ] 
 * - Adrian Pop, adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo
 * - creation 2007-02-24
 *   + interface for Windows and Linux sockets
 *   + all things in this file start with rmldb_socket_*
 * - last modified 2007-06-13
 *****************************************************************************/
#ifdef RML_DEBUG

#include "rml.h"
#include <errno.h>

int   rmldb_socket_debug = 0;

/* initialize the socket library */
int rmldb_socket_initialize_library(void)
{
#if defined(__MINGW32__) || defined(_MSC_VER)
  WSADATA info;
  if (WSAStartup(MAKEWORD(2,2), &info)) 
  {
    if (rmldb_socket_debug) 
      fprintf(stderr, "%s Error! WSAStartup failed: %s\n", 
      RMLDB_SOCKET_PROMPT, 
      strerror(_rmldb_socket_errorno));
    return -1;
  }
#endif
  return 0;
}

int rmldb_socket_cleanup_library(void)
{
#if defined(__MINGW32__) || defined(_MSC_VER)
	if(WSACleanup()) /* cleanup the socket library */
  {
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! WSACleanup failed: %s\n", 
				RMLDB_SOCKET_PROMPT, 
				strerror(_rmldb_socket_errorno));
    return -1;
  }
#endif
  return 0;
}

int rmldb_init_sockaddr (struct sockaddr_in *name, const char *hostname, int port)
{
  struct hostent *hostinfo = NULL;
  name->sin_family = AF_INET;
  name->sin_port = htons (port);
  hostinfo = gethostbyname (hostname);
  if (hostinfo == NULL)
  {
	  if (rmldb_socket_debug) 
      fprintf (stderr, "%s Error! Unknown host %s.\n", RMLDB_SOCKET_PROMPT, hostname);
    return -1;
  }
  name->sin_addr = *(struct in_addr *) hostinfo->h_addr;
  return 0;
}

int rmldb_socket_get(void)
{
  int sock = -1;

	if (rmldb_socket_debug) 
		fprintf(stderr, "%s Info! Obtaining a socket\n", RMLDB_SOCKET_PROMPT);

	sock = socket (AF_INET, SOCK_STREAM, 0);
	if (INVALID_SOCKET == sock)
	{
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! socket opening failed: %s\n", 
        RMLDB_SOCKET_PROMPT, strerror(_rmldb_socket_errorno));
	}
	if (rmldb_socket_debug) 
    fprintf(stderr, "%s Info! Socket %d obtained successfully\n", RMLDB_SOCKET_PROMPT, sock);

	return sock;
}

int rmldb_socket_bind_and_listen(int sock, char* hostname, int port)
{
	struct sockaddr_in server_address;
	memset(&server_address, 0, sizeof(server_address));	

	if (rmldb_socket_debug) 
    fprintf(stderr, "%s Info! binding and listen for socket %d on %s:%d\n", RMLDB_SOCKET_PROMPT, sock, hostname, port);

	rmldb_init_sockaddr(&server_address, hostname, port);
	
	/* bind */
	if (SOCKET_ERROR == bind (sock, (struct sockaddr *) &server_address, sizeof (server_address)))
	{
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! socket bind failed: %s. Is the bind port already taken?\n", 
				RMLDB_SOCKET_PROMPT, strerror(_rmldb_socket_errorno));
		return -1;
	}
	/* listen */
	if (SOCKET_ERROR == listen (sock, 1))
	{
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! socket listen failed: %s.\n", 
				RMLDB_SOCKET_PROMPT,	strerror(_rmldb_socket_errorno));
		return -1;
	}
	if (rmldb_socket_debug) 
		fprintf(stderr, "%s Info! socket bind and listen successful.\n", RMLDB_SOCKET_PROMPT);
	return 0;
}

int rmldb_socket_accept(int sock)
{
	struct sockaddr_in client_address;
	memset(&client_address, 0, sizeof(client_address));	
	int server_sock = RMLDB_FAILURE;
	unsigned int size = 0;
	if (rmldb_socket_debug) 
		fprintf(stderr, "%s Info! Calling accept on socket %d\n", RMLDB_SOCKET_PROMPT, sock);

	/* ----------------------------------------------------------- */
	/* now wait for connexions */
	/* ----------------------------------------------------------- */
	size = sizeof(struct sockaddr);
	if ((server_sock = accept (sock, (struct sockaddr*)&client_address, &size)) == INVALID_SOCKET)
	{
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! socket accept failed: %s.\n", 
				RMLDB_SOCKET_PROMPT, strerror(_rmldb_socket_errorno));
	}
	if (rmldb_socket_debug) 
		fprintf(stderr, "%s Info! socket opened and connected\n", RMLDB_SOCKET_PROMPT);
	return server_sock;
}

int rmldb_socket_close(int socket)
{
	int result;
	result = _rmldb_socket_close(socket);
	if (result == SOCKET_ERROR)
	{
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! socket closing failed: %s.\n", 
				RMLDB_SOCKET_PROMPT, strerror(_rmldb_socket_errorno));
	}
	if (rmldb_socket_debug) 
		fprintf(stderr, "%s Info! socket closed.\n", RMLDB_SOCKET_PROMPT);
  return result;
}

int rmldb_socket_connect(int sock, char* hostname, int port)
{
	struct sockaddr_in servername;
	rmldb_init_sockaddr (&servername, hostname, port);
	if (SOCKET_ERROR == connect (sock, (struct sockaddr *) &servername, sizeof (servername)))
	{
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! socket connect failed: %s. Is the external viewer started?\n", 
				RMLDB_SOCKET_PROMPT, strerror(_rmldb_socket_errorno));
    return -1;
	} 
	if (rmldb_socket_debug) 
		fprintf(stderr, "%s Info! socket opened and connected\n", RMLDB_SOCKET_PROMPT);
  return 0;
}

int rmldb_socket_outln(int sock, char *msg)
{
  if (rmldb_socket_out(sock, msg) < 0) return RMLDB_FAILURE;
  if (rmldb_socket_out(sock, "\n") < 0) return RMLDB_FAILURE;
  return RMLDB_SUCCESS;
}

int rmldb_socket_out(int sock, char *msg)
{
	int length = 0;
	length = _rmldb_socket_send(sock, msg, strlen(msg));
	if (length == SOCKET_ERROR)
	{
		if (rmldb_socket_debug) 
			fprintf(stderr, "%s Error! socket send failed: %s. Could not send: %s\n", 
				RMLDB_SOCKET_PROMPT,	strerror(_rmldb_socket_errorno), msg);
		return RMLDB_FAILURE;
	}
	if (length != strlen(msg))
	{
	  if (rmldb_socket_debug) 
		  fprintf(stderr, "%s Error! could not sent all into socket message: %s\n", 
			  RMLDB_SOCKET_PROMPT, msg);
		return RMLDB_FAILURE;
	}
	if (rmldb_socket_debug) 
    {
		fprintf(stderr, "%s Info! socket sent message: %s\n", RMLDB_SOCKET_PROMPT,	msg);
        fflush(stderr);
    }
  return RMLDB_SUCCESS;
}

#endif

