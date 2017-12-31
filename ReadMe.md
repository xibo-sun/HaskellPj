## Haskell Project
This is a project of Haskell course in Fudan University. The project is a FPS game.   
Author : Sun Xibo  
Mail   : 345567848@qq.com

## Building
================
1. Download source code:
 
		git clone git@github.com:HubertSun7/HaskellPj.git
		cd HaskellPj

2. Use cabal to build:  

    	cabal install

## Running
================
To run the game. Running the Server first:

		<Server_dir>/lambdaCSServer
		
Then use the Client:
 
		<Client_dir>/lambdaCS -t <server ip> -n <player name> 
		
And you should move the `./tga` directory to the `Client_dir`.

## Screenshot
Here's a screenshot
![screenshot][1].

[1]: https://raw.githubusercontent.com/HubertSun7/HaskellPj/master/screenshot.png