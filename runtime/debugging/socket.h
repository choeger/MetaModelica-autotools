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
 * [ rml-socket.h ] 
 * - Adrian Pop, adrpo@ida.liu.se, http://www.ida.liu.se/~adrpo
 * - creation 2007-02-24
 *   + interface for Windows and Linux sockets
 *   + all things in this file start with rmldb_socket_*
 * - last modified 2007-02-24
 *****************************************************************************/

#ifdef RML_DEBUG
#define RMLDB_SOCKET_PROMPT "rml+mmc-socket>"

#if defined(__MINGW32__) || defined(_MSC_VER)
/*********** MINGW32 && MSVC stuff **********/
#include <winsock2.h>

#define _rmldb_socket_send(x,y,z) send(x,y,z,0)
#define _rmldb_socket_recv(x,y,z) recv(x,y,z,0)
#define _rmldb_socket_close closesocket
#define _rmldb_socket_errorno WSAGetLastError()

#else /***************** unix stuff ***************/
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/unistd.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netdb.h>

#define _rmldb_socket_send(x,y,z) write(x,y,z)
#define _rmldb_socket_recv(x,y,z) read(x,y,z)
#define _rmldb_socket_close close
#define _rmldb_socket_errorno errno
#define SOCKET_ERROR (-1)
#define INVALID_SOCKET (-1)

#endif

/* 
 * set to 1 to have debugging or to 0 to 
 * disable debugging of the socket communication
 */
extern int   rmldb_socket_debug;
/* 
 * initialize the socket library
 * returns -1 on error and 1 on success.
 */
extern int   rmldb_socket_initialize_library(void);  
/* 
 * cleanups the socket library
 * returns -1 on error and 1 on success.
 */
extern int   rmldb_socket_cleanup_library(void);  

/* 
 * get a socket 
 * returns -1 or the socket
 */
extern int   rmldb_socket_get(void);

/* 
 * binds the given socket on localhost and given port
 * performs bind and listen
 * returns -1 on error and 0 on success
 */
extern int   rmldb_socket_bind_and_listen(int socket, char* hostname, int port);
/* 
 * performs accept on the given socket 
 * returns the new socket or -1 on error
 */
extern int   rmldb_socket_accept(int socket);

/* 
 * close the socket 
 * returns -1 for error or 0 for no error 
 */
extern int   rmldb_socket_close(int socket); 

/* send to socket with error handling */
extern int rmldb_socket_outln(int sock, char *msg);
extern int rmldb_socket_out(int sock, char *msg);

#endif