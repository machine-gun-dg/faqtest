# Frequently Asked Questions (FAQ)

## FAQ Index
[OVERVIEW](#OVERVIEW)
[GENERATED-CODE](#GENERATED-CODE)
[THIRD-PARTY-COMPONENETS](#THIRD-PARTY-COMPONENETS)

## OVERVIEW

### What is the NIB framework?
We use a dedicated Java/.NET framework. The framework is a set of pre-built Java/.NET components leveraging modern development practices and tools that support the lifecycle of COBOL/PLI/ASM applications recompiled/refactored to Java/.NET. The framework consists of three main components: 
#### i) Commons: a collection of common packages and utilities for the converted programs (program management, storage management, file i/o, E/SQL...)
#### ii) Supernaut: to replace mainframe TP monitors (CICS/IMS TM) 
#### iii) Motorhead: to replace the IBM mainframe batch engine. 
Framework Advantages: 
<br> i) Reduction the amount of generated code, 
<br> ii) Best Practices and Standards, 
<br> iii) Security and Stability, 
<br> iv)  Integration and Interoperability, 
<br> v) Scalability and Performance. 

## GENERATED-CODE

### What is the Java version used?
We typically use the latest version available at the time of the project. 
The minimum JDK required is 17. 

## THIRD-PARTY-COMPONENETS

### What are the Java framework used?
There are a variety of frameworks that we can use but primarily we use Spring. An accurate list of framework depends on the specific configuration that will be adopted for the specific project. 

### What is the deployment architecture? Is it Container based? 
NIB can use container as well as as bare metal and virtual machine or a combinations of all

### Does target architecture follow Microservice Architecture?
The architecture is cloud native and does not offer any limitation to develop micro services. The migration we perform is typically 1:1 and a transition to micro services is typically part of a second leg of the modeenization journey. 

### Are there any Vendor specific libraries/SDK that will need to be part of code? What will be the maintenance or upgrade path for it? 
NIB proposal is very flexible and can offer a no vendor lock in proposal.
Maintenance and Support for the NIB framework that can be provided in various formats

### What are the integration endpoints supported, i.e. APIs / Messaging? 
NIB can implement interfaces of any kinds. NIB comes with a number of already defined interfaces for MQ for examples and others. 
Ad hoc - specific integrations can be devolepd as needed 

### What is the Data Format (JSON / XML…) for APIs?
JSON	

### What are the alternatives of powershell scripts against for the JCL conversion for .NET target? 
Groovy can be provided even for .NET

### While migrating BMS to Angular can you change the layout automatically?
Yes, we can automate some layout changes within limits (apply logos, fontstyle, colors, footers can be easily automated, consolidate multiple maps, change maps layout are more difficult and require a dedicate analysis)

### Does NIB offer support for Web Services?
Generally yes,  however programs to be exposed as Web Services require that the Cobol programs are coded for that, transforming existing CICS programs using BMS maps to WS logic requires a reengineering step.

### How do you handle state management in the containerized environment?
State management for DB2/IMS DB is handled by the target database. For the files the state management is guaranteed by the target  shared file system (i.e. AWS EFS, FSx o EBS)

### How do you manage database connections and transactions in a Docker / Kubernetes environment?
Exactly like any other Java/.NET application: 
####Java:
<br>- JDBC: We will utilize JDBC to connect to the database and execute SQL queries.
<br>- JTA: For distributed transactions, we will use JTA to coordinate transactions across multiple data sources, ensuring data consistency.
####.NET:
<br>- ADO.NET: to connect to the target DB and execute SQL queries 
<br>- TransactionScope: for distributed transactions we use TransactionScope class part of .NET or, if available, the  TransactionScope class provided by the target DB

### In case of Kubernetes do we need a shared volume in Kubernetes?
If the customer requires to share files across the batch nodes, all the nodes must be connected to the same storage. If it's a shared k8s volume or any other solution (i.e. plain old NFS) it is fine. Of course, performance must be considered when designing the architecture. 

### In Kubernetes, there is a concept of a Job, which is designed for 'fire-and-forget' tasks, meaning it runs a specific workload to completion and then terminates. On the other hand, Kubernetes Services are typically used to expose and manage long-running workloads, such as applications or APIs, that need to be continuously available and accessible. If we decide to deploy our application or workload in Kubernetes, shall we use Kubernetes Jobs or Kubernetes Services? 
Services

### What are the z/OS INTERCOMMUNICATION services that are supported by your solution?
NIB fully supports  TCP/IP based protocols: HTTP, FTP, Web Services and MQ. 
<br>NIB exposes TN3270 over IP but do not consume it (we are servers not clients).
<br>About files exchange we rely on system's product for the file transfer. So we support FTP, SFTP,FTPS, SPAZIO, XCOM...
<br>CTG / IMS Connect - only partially supported and requires specific analysis

### Could you please specify more about your MQ support?
NIB supports MQ through JMS (Java Message Service) and IBM MQ Client for Java (or similar like RabbitMQ). Therefore whatever is supported by the latter is supported by NIB.


TP is built on Redis. Redis is: i) by design implemented to be horizontally scalable ii) utilized to store and manage all CICS session data  iii) available through all major cloud vendors as a managed service. There is no mandatory requirement to use an application server (AS). AS is used (if required) to handle services like CTG, CICS connect. Redis can be used standalone or with an AS to further enhance performances, scalability etc. based on customer requirements. The TP Framework is also providing 3270 as well as Angular services. For the batch side our solution is based on XML with Spring Batch or Groovy. Groovy is a modern scripting language similar to Java, more concise and expressive than XML and is right now the primary suggested solution given that the roadmap for Spring Batch future version 5 (expected in 2024) is shifting the focus towards Groovy instead of XML. Groovy relies on a component called Motorhead that acts as batch orchestrator to schedule certain classes or specific jobs on specific batch engines, VM, containers…

### 2. How do I install [Project Name]?
To install the project, follow these steps:
1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/yourrepository.git
