# Frequently Asked Questions (FAQ)

## FAQ Index
<br>[1-OVERVIEW](#1-OVERVIEW)
<br>[2-NIB_REQUIREMENTS](#2-NIB_REQUIREMENTS)
<br>[3-DATA_FILES](#3-DATA_FILES)
<br>[4-CONTAINERS](#4-CONTAINERS)
<br>[5-SECURITY](#5-SECURITY)
<br>[6-2PC](#6-2PC)
<br>[7-SAS](7-#SAS)
<br>[MISC](#MISC)

## 1-OVERVIEW

### + What is NIB?
NIB is a comprehensive tool designed to automate the migration and modernization of legacy applications. It comprises three major components:
<br> • **Automatic Converter:** is a key feature utilized primarily by NIB team. It enables users to automatically migrate source components to a desired target language, such as COBOL, ASM, Easytrieve, PLI, SAS to Java or .NET. Additionally, it supports code migration from databases like Db2, IMS DB, and VSAM to various RDBMS solutions. The JCL (Job Control Language) is automatically migrated to Groovy or PowerShell, depending on the target environment.
<br> • **Integrated Development Environment (IDE):** The IDE is a plugin compatible with widely-used IDEs, allowing developers to maintain applications converted to the target language. Supported IDEs include IntelliJ IDEA and Visual Studio Code (VSC), ensuring a familiar and modern development environment for users.
<br> • **Runtime Environment / Framework:** a collection of libraries that support the execution of converted components in the new environment. 
<br>Together, these components empower organizations to transition from outdated programming languages and environments to modern technologies seamlessly.

### + NIB High Level Architecture
NIB is designed with a **cloud-native architecture**, capable of being containerized. This allows it to run across various environments, including major cloud providers, virtual machines, and on-premises setups, including hybrid scenarios.

### + What is the NIB framework?
We use a dedicated Java/.NET framework. The framework is a set of pre-built Java/.NET components leveraging modern development practices and tools that support the lifecycle of COBOL/PLI/ASM applications recompiled/refactored to Java/.NET. The framework consists of three main components: 
<br> 1 **Commons:** a collection of common packages and utilities for the converted programs (program management, storage management, file i/o, E/SQL...)
<br> 2 **Supernaut:** to replace mainframe TP monitors (CICS/IMS TM) 
<br> 3 **Motorhead:** to replace the IBM mainframe batch engine. 
<br> Framework Advantages: 
<br> • Reduction the amount of generated code, 
<br> • Best Practices and Standards, 
<br> • Security and Stability, 
<br> • Integration and Interoperability, 
<br> • Scalability and Performance. 

### + How are BMS/MFS supported?
NIB **supports 3270 streams**, allowing customers to utilize their preferred 3270 clients while preserving their existing scripts and macros.
<br>Additionally, NIB can automatically convert BMS/MFS into online maps, rendering them as web pages with frontend libraries such as **Angular**. The appearance can be easily customized to replicate the 3270 interface or modernized to improve the user experience.
<br>**Both 3270 streams and Angular versions can coexist simultaneously.**

### + How are JCL migrates?
NIB automatically migrate **JCLS into Groovy** (for Java target) and **PowerShell** (for .NET target)

### + What is the Java version used?
NIB typically uses the latest version available at the time of the project. 
**The minimum JDK required is 17.**

### + What are the Java framework used?
There are a variety of frameworks that we can use but primarily we use **Spring Boot**. 
<br>The major external libraries used are:
<br> • **Spring Framework** a comprehensive and popular platform for building Java applications, providing support for widely used architectural patterns and components.
<br> • **Apache Commons** a collection of reusable Java components that address common programming patterns.
<br> • **Google Guava** set of core Java libraries from Google, providing utilities for collections, caching, concurrency, and more.
<br> • **Netty** High-performance asynchronous event-driven network application framework.
<br> • **Atomikos** Provides transaction management and connection pooling for Java applications, primarily focused on distributed transactions to ensure data consistency across multiple databases or systems, even in case of failures.
<br> **NOTE:** an accurate list of framework depends on the specific configuration that will be adopted for the specific project. 

### + What is the deployment architecture? Is it Container based? 
NIB can use container as well as as bare metal and virtual machine or a combinations of all

## 2-NIB_REQUIREMENTS

### + Target - VM Hardware / OS
 • **CPU** >= Min i7 or similar
<br> • **Memory** >= 8GB 
<br> • **Disk Space** >= 2GB (SSD)
<br> • **O.S.** >= Windows 10

### + Software
#### Mandatory
 • **JDK** 17.0 or later 
<br> • **IDE**
<br> - IntelliJ IDE Latest Version OR
<br> - Eclipse IDE Latest Version 
<br> • **Target RDBMS** (e.g. SQL Server) Latest version
<br> • **Sourve Code Versioning** GitLab or equivalent to keep the baseline and versioning of the migrated components
<br> • **Text Editor** Notepad++ as free editor with utilities to work with code
<br> • **Database Manager** Heidi or equivalent
#### Optional
 • **Docker** latest version
<br> • **Compare Utility** Beyond Compare
<br> • **Spreadsheet** Excel or equivalent
<br> • **MySql Server >= Version 5**  (required for the assessment)
#### Grants / Permissions
 • Access to customer sources and data
 <br>• Access from/to GitLab or equivalent
 <br>• Access to maven to ensure all dependencies required by NIB tooling can be downloaded


## 3-DATA_FILES

### + How the VSAM migration work? 
We have 2 approaches:
<br>1) **Default approach** – we are extremely quick data is migrated with no issues to the target DDL (described above)
<br>2) **Smart Approach** – this approach does have both prep time (to properly define the remapping rules) and migration time (to cleanse some data issue that do occur in all the project we have done in the past, so they are not an exception). 

### + How does the conversion of ESDS, KSDS and RRDS VSAM files works?
NIB does support ESDS, KSDS as well as RRDS. NIB has automatic handling of SEQ NO within the NIB framework. That applies to ESDS as well RRDS but it is not required for KSDS. In the demo we can show how we convert a VSAM (KSDS as this is the version used in AWS Card Demo App) using a key field and a var binary (default). Of course, we can also choose a “smart conversion” with proper columns but that process does require more time as customer must be involved (e.g. date/time transformation rules must be agreed upon) as well as data cleansing that is most likely going to be required. To complete the answer the actual original COBOL call will not change as there will be VSAM functions (like READ, WRITE... just in java) that will be interpreted by our framework (hence behind the scenes) with appropriate SQL statements (e.g. READ = SELECT, WRITE = INSERT....)

### + What are the performance in migrating VSAM to RDBMS?
As reference on our little demo server we completed a KSDS VSAM of 1M records to PGSQL in 39 seconds using the default approach…. It could be faster depending on the number of horse powers we can count on

### + How does NIB handle SORT steps in a batch job dealing with VSAM?
SORT is a powerful utility that offers a wide range of features, including filtering, conditions, merging, and more. However, its complexity can make it challenging to configure, especially when working with VSAM files. To simplify the process and ensure efficient data management, our solution -in similar scenarios- focuses on sequential files (VSAM are transformed to sequential). This approach eliminates the complexities associated with SQL interactions and binary or packed data formats, which would require additional operations like SUBSTR and could potentially lead to incorrect sorting. By working with sequential files, we provide a simpler and more efficient sorting process. Our decision to avoid SQL support was driven by our desire to maintain a lightweight architecture focused on the specific needs of our product.

### + How does NIB support GDGs?
GDGs are essentially regular files with an added concept of levels, such as FILENAME-LEVEL01, 02, 03... In the target environment, the solution will preserve the same number of levels as on the mainframe. When the maximum level is reached, the data will overwrite the initial level, maintaining the same functionality as on the mainframe. The logic of levels is handled entirely by our framework

## 4-CONTAINERS

### How does NIB manage database connections and transactions in a Docker / Kubernetes environment?
Exactly like any other Java/.NET application: 
#### Java:
<br>- JDBC: We will utilize JDBC to connect to the database and execute SQL queries.
<br>- JTA: For distributed transactions, we will use JTA to coordinate transactions across multiple data sources, ensuring data consistency.
####.NET:
<br>- ADO.NET: to connect to the target DB and execute SQL queries 
<br>- TransactionScope: for distributed transactions we use TransactionScope class part of .NET or, if available, the  TransactionScope class provided by the target DB

### + How does NIB handle state management in the containerized environment?
State management for DB2/IMS DB is handled by the target database. For the files the state management is guaranteed by the target  shared file system (i.e. AWS EFS, FSx o EBS)

### + In case of Kubernetes do we need a shared volume in Kubernetes?
If the customer requires to share files across the batch nodes, all the nodes must be connected to the same storage. If it's a shared k8s volume or any other solution (i.e. plain old NFS) it is fine. Of course, performance must be considered when designing the architecture. 

### In Kubernetes, there is a concept of a Job, which is designed for 'fire-and-forget' tasks, meaning it runs a specific workload to completion and then terminates. On the other hand, Kubernetes Services are typically used to expose and manage long-running workloads, such as applications or APIs, that need to be continuously available and accessible. If we decide to deploy our application or workload in Kubernetes, shall we use Kubernetes Jobs or Kubernetes Services? 
Services


## 5-SECURITY

### + What is the NIB authentication mechanism for APIs OOTB (out of the box)?
NIB does not have a specific preference for the authentication mechanism; NIB can interface (with an ad hoc interface) any available authentication method requested by the client.

### + What are the security standards applied? 
The whole legacy application security logic (RACF, CICS, IMS etc.) is migrated to equivalent security features available in the target architecture and handled using Java interfaces, NIB does not have a preferred framework for implementing the actual security control, we are flexible in that sense and can easily accommodate any customer preference as far as the selected replacement provides with all the necessary features. About the system security (file access, source code access, deployment processes etc.) that part is related to the infrastructure architecture and the access standards together with rules and roles will be defined in the SOW.
Application security is migrated as-is.

### + Does NIB refactor the RACF components (service accounts/user profiles) and take care in the ASP.NET code? For example, when we read files, file access & resposition is created thru RACF. But in ASP.NET we have to add extra code on authorization call to Azure AD . Does this is done automatically during refactoring process? 
We can convert RACF to AD but this is not part of our standard proposal and this requires some special considerations given security migration can only be partially automated 

## 6-2PC 

### + Does the NIB framework support two phase commit?
Yes, it does. In particular:
<br>• **Java**
Java is leveraging Atomikos, a popular open-source transaction management framework that supports distributed transactions across multiple resources, such as databases and message queues.  Atomikos implements the 2PC protocol to manage distributed transactions. It coordinates the transaction across multiple resources, ensuring that all resources either commit or roll back changes.
<br>• **.NET**
For the transactions in .NET, NIBs use the Microsoft package "System.Transactions" and in particular  CommittableTransaction that handles all open connections. Any data source in "enlisted" like for example a ADO connection to SQL Server, can participate to the transitions. In simple terms NIB demands all transaction handling as well as the 2PC to the MSFT runtime.

### + What happens when we have a transaction that is both DB and MQ, do we support the single unit of work? and is there any difference if the process is a transaction or a batch process?
Same as on mainframe. On the mainframe: 
<br>i) Online transactions are managed by CICS. 
<br>ii) Batch transactions must be managed programmatically either calling DB or MQ API or using the RRSAF API, like DSNRLI.
<br>NIB is behaving pretty much the same:
<br>• **Online**
<br> Java - transactions are managed by Atomikos (https://www.atomikos.com) that is integral part of our solution so whatever components provides an XA driver can join 2PC transactions. Specifically, DB and IBM MQ can do that.
<br> .NET - transactions are managed by "System.Transactions" and in particular  CommittableTransaction that handles all open connections
<br>• **Batch**
<br> Java / .NET
we can invoke driver's API to manage transactions. 
<br> For the sake of transparency, we do not provide RRSAF equivalent API, but a similar functionality can be replicated using Atomikos/CommittableTransaction.

### Are "in-flight" transactions automatically rolled-back by the surviving pods?
They are simply rolled back by the transaction manager. The "surviving" pods don't care of in-flight transactions of the dead one. It's the transaction agent (i.e. the DB) that does not receives a commit therefore rolls back.

### JMS (Java Message Service) and JDBC are typically coordinated by JTA (Java Transaction API to allow distributed transaction) to provide 2PC. Does NIB do the same?
Yes, NIB uses Atomikos.



## 7-SAS

### Do we support pac decimal fields in SAS?
Yes, we do, i.e.:  
@110 ST1MI     PD3.  -> .add(V_ST1MI, 110, SasFormat.PackedDecimal, 3, 0)

### Do we support EBCDIC?
Yes, as an option in our SAS runtime

### Do we have entry points in our SAS programs to interface JCLs?
Yes, we do use a customizable SAS launcher wrapper for starting our converted SAS programs that simplify the interfacing with third party framework (like BAge and CTU from IBM)

### What information do we need to know to interface with a third party framework?
We only need access to the DD info of the step where the converted SAS program is started

### in our SAS solution do we support any data source? For example what about VSAM? How do we integrate with the way a third party framework migrates VSAMs? And how we return the data in the exact same format if the SAS expected output is VSAM?
For external I/O (i.e. VSAM) we have interfaces to be implemented with the the actual framework used, we let the framework accessing the actual data

### How do we handle failures/exceptions? What happens if there is a failures/exception in a SAS programs? How do we communicated that to the third party Framework?
We have interfaces for logging and exceptions that can be customized to trap any exception or/and error message

### How we can understand the format of the files used in SAS? There is a SAS catalog but do we use it? IF yes how can we identify if a file is a VSAM, Db2 Table, GG, sequential....?
We rely on the framework starting the converted SAS program to get the info about the files being used in the step, all the file info are injected into the SAS runtime before starting the converted SAS from our SAS wrapper

### What info NIB runtime need to know to integrate with the target framework other that NIB?
Given we have not seen SAS being called by anything else than JCLs all Q&A above should be enough


## MISC 

### What type of load balancer we need for example level 7 or level 4?
For the TN3270 protocol (used by online terminals), we utilize a Network Load Balancer (NLB) operating at Layer 4. For all other use cases, including online and batch APIs as well as web services, an Application Load Balancer (ALB) operating at Layer 7 is sufficient. 

### How much Unit Testing and Functional test automation is provided? 
Testing scope extends beyond standard unit and functional testing. We conduct comprehensive pre-delivery testing based on client-provided test cases. These end-to-end tests simulate real-world scenarios, including JCL execution (when feasible), COBOL program submission, database interactions, and integrations with systems like MQ. By producing reports and updating database tables, we verify system functionality. Our largely automated migration process allows for efficient validation with a relatively small test suite. Due to the consistency of our automation, successful test cases often indicate broader code correctness for similar functionalities and patterns.

### What are the test tools used? 
For testing we can primarily generated automated Junit tests during the migration that will represent a start point for the subsequent testing (with manual effort required)

### How to integrate scheduler with NIB?
The scheduler will schedule jobs, but we have a 'load balancer' component that can distribute these jobs across different batch engines. A batch engine can be a process within a container, a separate virtual machine, or a physical server. Typically, the distribution is based on message class. However, we can also customize it to a more granular level. For instance, we can define multiple Class A engines for high-priority jobs and assign jobs starting with 'XX' to engine A-1, jobs starting with 'YY' to engine A-2, and so on. 
The load balancer sits between the scheduler and the engines. It doesn't affect the scheduler's configuration beyond the initial setup of the load balancer itself.
It's worth noting that, independently of the scheduler and on-demand (e.g., using the Grafana dashboard provided by NIB standard package), batch engines can be activated, deactivated, and jobs can be manually submitted or restarted to any engine, even if it's in a different class.

### Is an application server required?
NIB solution is a based on Spring. Therefore it can be used inside any AS that supports Spring. However, with our solution, it is not necessary to use an Application Server. Our preferred solution is without AS. We deploy 100% Spring apps and as such does not require an AS.

### When a job is submitted. What does actually happen? <br>i) a new process is started as JVM with Spring Boot and the application code (groovy then Java). At the end of the JOB execution the JVM is stopped? <br>ii) It trigers a program inside an already up&running JVM with Spring boot?
We have both options. We can either:
<br>i) run a groovy script directly, with a JVM starting and stopping as you described
<br>ii) use our "JES like" framework (Motorhead). It's a cluster of Spring nodes (containers, VMs, etc.) that takes care of executing scripts in a JES like fashion. So the JVM starts when the node starts and when a job is submitted it is assigned and executed by (one thread on) one of the running JVM. Once it is done, the script is disposed and the JVM is available for the next one. Moreover nodes can be configured with one or more threads, so each JVM can run, isolated, multiple parallel jobs. But this is a pure configuration decision.
mLogica preference is "JES like" option ii)

### Do you support multithreading?
if by multithreading we mean if we can start parallel processes then the answer is yes

### + Does target architecture follow Microservice Architecture?
The NIB architecture is **cloud-native** and imposes no limitations on developing microservices. 
Migration is preferably conducted as a 1:1 iso-functional transformation. Transitioning to microservices is typically addressed as part of the second phase of the modernization journey.

### + Are there any Vendor specific libraries/SDK that will need to be part of code? What will be the maintenance or upgrade path for it? 
The NIB proposal is highly flexible and can be provided in various formats, including a no vendor lock-in option that includes full access to the NIB framework source code. 
Subscription models are also available, along with permanent licenses that include maintenance and support.

### + What are the integration endpoints supported, i.e. APIs / messaging? 
NIB can implement interfaces of any kinds. NIB comes with a number of already defined interfaces for MQ for examples and others. 
Ad hoc - specific integrations can be devolepd as needed 

### + What is the Data Format (JSON / XML…) for APIs?
JSON	

### + What are the alternatives of powershell scripts against for the JCL conversion for .NET target? 
Groovy can be provided even for .NET

### + While migrating BMS to Angular can you change the layout automatically?
Yes, NIB can automate certain layout changes within specific limits. For instance, applying logos, font styles, colors, and footers can be easily automated. 
However, consolidating multiple maps and altering map layouts are more complex tasks that require dedicated analysis and migration services. 

### + Does NIB offer support for Web Services?
Generally, yes; however, for programs to be exposed as Web Services, the original COBOL logic must be specifically coded to support this functionality. 
If not, transforming existing CICS programs that utilize BMS maps into Web Service logic may require dedicated reengineering steps. 


### What level of logging and tracing that will be available in solution code flow? 
NIB provides applicative logging capabilities (e.g., Logback), allowing you to customize logging based on your specific requirements. 
In addition, NIB offers Micrometer for application metrics. While we demonstrate Micrometer data with Grafana in the demo, we expect you to use your preferred industry-standard telemetry tool like Dynatrace or AppDynamics. Micrometer's collected data can be easily integrated with these tools.
The equivalent of Java/Micrometer in .NET is .NET/OpenTelemetry

###  Is dynamic reloading of program supported ? (how is handled a new compiled version deployment)
Once a program is compiled the equivalent .class is generated.
The mLogica framework uses a standard Java class loader to load classes into the JVM. 
When a specific class is needed, the class loader checks if it has already been loaded. 
If not, it loads the class from a specified location.
The above applies for both COBOL and Java components (previous COBOL, PLI, etc.. refactored to Java)


### What is the best/recommended IDE to develop with Liber*M software ?
We suggest:
<br>•	IntelliJ IDEA for Java and 
<br>•	Eclipse for COBOL and/or Java


### How is the transactional monitor configuration captured / migrated (e.g. CICS CSD config file)?
Each CICS region (called Supernaut Region) is configured using the CSD files configuration from the mainframe.
CSD is automatically converted in corresponding JSON and YAML config files.

### What are the z/OS INTERCOMMUNICATION services that are supported by your solution?
NIB fully supports  TCP/IP based protocols: HTTP, FTP, Web Services and MQ. 
<br>NIB exposes TN3270 over IP but do not consume it (we are servers not clients).
<br>About files exchange we rely on system's product for the file transfer. So we support FTP, SFTP,FTPS, SPAZIO, XCOM...
<br>CTG / IMS Connect - only partially supported and requires specific analysis

### Could you please specify more about your MQ support?
NIB supports MQ through JMS (Java Message Service) and IBM MQ Client for Java (or similar like RabbitMQ). Therefore whatever is supported by the latter is supported by NIB.

### Does NIB have a notion of a File Catalog? Where the catalog resides?
Yes, the catalog is stored in the database. 

### Data Types: COMP-3, Zone Decimal – how those are implemented/supported?
See an example of COBOL and equivalent Java being generated 
In our framework we have the concept of NPacked class that define the size of the variable and as well as the sign (true when it is signed).
In essence we retain the same format that was available on mainframe.
See a snippet.
```Java
// COB:            05 WS-LINE-COUNTER    PIC 9(09) COMP-3
// COB:                                             VALUE 0.
public NPacked wsLineCounter = new NPacked(5, false).initial(0);
```

### How are redefines mapped to Java? Can you provide some complex examples?
Please see a COBOL snippet
```COBOL
05 CICS-OUTPUT-EDIT-VARS.
           10  CUST-ACCT-ID-X                      PIC X(11).
           10  CUST-ACCT-ID-N REDEFINES CUST-ACCT-ID-X
                                                   PIC 9(11).
           10  WS-EDIT-DATE-X                      PIC X(10).
           10  FILLER REDEFINES WS-EDIT-DATE-X.
               20 WS-EDIT-DATE-X-YEAR              PIC X(4).
               20 FILLER                           PIC X(1).
               20 WS-EDIT-DATE-MONTH               PIC X(2).
               20 FILLER                           PIC X(1).
               20 WS-EDIT-DATE-DAY                 PIC X(2).
           10  WS-EDIT-DATE-X REDEFINES
               WS-EDIT-DATE-X                      PIC 9(10).
           10  WS-EDIT-CURRENCY-9-2                PIC X(15).
           10  WS-EDIT-CURRENCY-9-2-F              PIC +ZZZ,ZZZ,ZZZ.99.
```
And equivalent Java generate
```Java
    public static class CicsOutputEditVars extends NGroup {
      public NChar custAcctIdX = new NChar(11);
      public NZoned custAcctIdN = new NZoned(11, false).redefines(custAcctIdX);
      public NChar wsEditDateX = new NChar(10);
      public static class Filler362 extends NGroup {
        public NChar wsEditDateXYear = new NChar(4);
        public NChar filler364 = new NChar(1);
        public NChar wsEditDateMonth = new NChar(2);
        public NChar filler366 = new NChar(1);
        public NChar wsEditDateDay = new NChar(2);
      }

      public Filler362 filler362 = (Filler362) new Filler362().redefines(wsEditDateX);
      public NZoned wsEditDateX_ = new NZoned(10, false).redefines(wsEditDateX);
      public NChar wsEditCurrency9_2 = new NChar(15);
      public NZoned wsEditCurrency9_2F = new NZoned(15).formatAs("+###,###,###.00");
    }
```

### How you support include files, like COPY, especially when the COPY contains block of code.
As a general rule:
<br>•	The copy of data data are generated in a separate class
<br>•	The copy procedures are expanded in the main class

### Alphanumeric comparisons: in ASCII context, how EBCDIC comparisons are implemented in java?
If we retain the EBCDIC format, no changes.
<br>If we go to ASCII the collating sequence will be ASCII.
<br>If for whatever reason we need to retain the EBCDIC collating sequence as that may affect the downstream process we will need to re-engineer the programs

### Is pointer arithmetic supported?
Yes, it is Supported in a similar fashion as on mainframe.  Please see a snippet.
```Java
  // COB:        01 WS-JOBNAME-PTR                USAGE POINTER.
  public NPointer wsJobnamePtr = new NPointer();
```
Please note we do rely on a special NPointer to manage pointers.

### How do you handle COBOL GOTOs?
We do eliminate GOTO and GOBACK as well as PERFORM THRU.
In the generated Java we adopted the concept of *“Gravity Flow”*.
So Instead of jumping around with GOTO (or Break/Continue statement in Java), our converted structures the generate Java code to follow a linear, top-to-bottom flow. 
We use a special variable called rcNext (or rcPrev) that is able to track the current state or flow of execution.
rcNext is part of a loop cycle that will be executed till the rcNext hits the “Exit” condition.
In the loop cycle there is a switch statement that acts as a dispatcher, determining which block of code to execute based on the value of rcNext (or rcPrev). 
Each case corresponds to a specific flow.

### In the case of Non VSAM files, how did you control the locking between multiple processes that want to access the same file?
We have two mechanisms in place:
<br>1) *Logical Lock*: When operating under Motorhead (our equivalent to JES), the job "locks" the dataset similarly to JES. This is a logical lock that mimics the JES approach.
<br>2) *Physical Lock*: At the time of opening, the file is accessed with the necessary lock mode.
