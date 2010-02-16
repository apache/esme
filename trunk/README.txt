===========================================================
Welcome to Apache ESME  <http://incubator.apache.org/ESME/>
===========================================================

Enterprise Social Messaging Experiment (ESME) is a secure and highly 
scalable microsharing and micromessaging platform that allows people to 
discover and meet one another and get controlled access to other sources 
of information, all in a business process context.   


Apache ESME is an effort undergoing incubation at The Apache Software
Foundation (ASF), sponsored by the Apache Lucene PMC. Incubation is
required of all newly accepted projects until a further review indicates
that the infrastructure, communications, and decision making process have
stabilized in a manner consistent with other successful ASF projects.
While incubation status is not necessarily a reflection of the completeness
or stability of the code, it does indicate that the project has yet to be
fully endorsed by the ASF.

See http://incubator.apache.org/projects/ESME.html for the current
incubation status of the Apache ESME project.

License (see also LICENSE.txt)
==============================

Collective work: Copyright 2007 The Apache Software Foundation.

Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Building ESME
=============

There are two directories in this release. The first directory "server" 
contains the main "ESME" server. the second directory "esme-java-client" 
contains an example of a java client that is using the our original REST
API.

Please note that ESME currently only works with JDK 1.6. We are hopeful
that JDK 1.5 will soon be supported.

You can build the ESME sources using the Maven 2 build system. Execute the
following command in the ESME source directory to build the sources and
to start the application via jetty:

    mvn jetty:run

See the Maven web site at http://maven.apache.org/ for more instructions
and the latest Maven downloads. 

Mailing Lists
=============

Discussion about the ESME project takes place on the development mailing
list ESME-dev@incubator.apache.org. The list is open to anyone and
publicly archived. You can subscribe the mailing list by sending a
message to ESME-dev-subscribe@incubator.apache.org, and unsubscribe by
sending a message to ESME-dev-unsubscribe@incubator.apache.org. To receive
more instructions, send a message to ESME-dev-help@incubator.apache.org.

Issue Tracker
=============

If you encounter errors in ESME or want to suggest an improvement or
a new feature, please visit the ESME issue tracker at
https://issues.apache.org/jira/browse/ESME. There you can also find the
latest information on known issues and recent bug fixes and enhancements.

