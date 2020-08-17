CMProject

An Open Source Lazarus-Mormot framework ready for rich applications.

(c) 2020 Claudio Martín

Contributors
  Claudio Martín
  Arnaud Bouchez - http://blog.synopse.info  

CMProject is an Open Source project for Lazarus, targeting Windows
systems ( looking for contributors for other platforms ) for build ready
to use desktop applications, in client or clientserver mode. Including a 
server to be installed as a service when necessary, fully in tune with 
Synopse mORMot framework.

The proposal is to have a project template, which allows generating an 
"tabbed interface" application with a main screen, with a top bar for main 
actions, and a vertical menu in tree, that lunches subsecuents screens in 
tabs or modal mode, according to the needs.

 ------------------------------------------------------------
| Btn1 | Btn2 | .........| Btn Exit |               Info.... |
 ------------------------------------------------------------|
| Menu finder     |/  Manin  \/ Menu1.1 x \/ Menu2.1 x \     |
------------------ -----------             ------------------|
| Mod Menu 1      |                                          |
|   Menu1.1       |                                          |
|   Menu1.2       |                                          |
| Mod Menu 2	  |                                          |
|   Menu2.1       |          Functionaliy of Menu1.1         |
|   Menu2.2       |                                          |
|   Menu2.3       |                                          |
|                 |                                          |
|                 |                                          |
|                 |                                          |
|                 |                                          |
|                 |                                          |
--------------------------------------------------------------

VirtualTreeView needs to be installed, as the menu and other future options 
are based on this component.

One of the main goals is to just rely on Lazarus, VirtualTreeView and 
Syopse mORMot framework.

The repository contains a TestProject folder which shows how to get starting.

