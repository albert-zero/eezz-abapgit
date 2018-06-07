This project installs HTML as user interface to ABAP projects using 
WEB sockets (ABAP push channel). 

The output is event driven and could be designed without Java-Script.
To run this project you would need to create a push channel and hook into the zcl_eezz_agent.
Import the classes and the repository objects into BSP bibliothek.

Run the index.html, which displays the SFLIGHT table.
    
For further documentation see PDF in project EezzServer <download>/EezzServer/webroot/repository

Special features:
=> Fast access to huge data sets using virtual tables
=> Fast development: No framework, no JavaScript, strictly decoupling UI and app-implementation
