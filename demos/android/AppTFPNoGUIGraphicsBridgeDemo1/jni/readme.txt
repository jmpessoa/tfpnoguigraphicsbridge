TFPNoGuiGraphicsBridge

	ref. https://github.com/jmpessoa/tfpnoguigraphicsbridge

	"A wrapper over Free Pascal fcl-image ref. http://wiki.freepascal.org/fcl-image".

	"FPImage (fcl-image) draw images which won't be displayed in the screen [NoGUI !!!]. 
	A program, for example, running on a webserver without X11 could benefit from 
	not having a visual library as a dependency." 
		ref. http://wiki.freepascal.org/Developing_with_Graphics


version: 0.2 - 22 June - 2017

Author:

	Jose Marques Pessoa

	jmpessoa_hotmail.com

	ref. https://github.com/jmpessoa/tfpnoguigraphicsbridge

1) WARNING [windows]: Before install the component TFPNoGuiGraphicsBridge:

	Copy: "freetype-6.dll" and "zlib1.dll" 
		FROM: component sub-folder "dll_stuff_win32"
		TO: folder "C:\windows" or "C:\windows\system32" [win32]  or  C:\Windows\SysWOW64   [win64]

2) Android LAMW project: 
     
		ref. https://github.com/jmpessoa/lazandroidmodulewizard

	2.1) Cross-compiling TFPNoGuiGraphicsBridge for "arm-android" or "x86-android" [Required for first use]

	a) Create a new LAMW [GUI] project
	b) Go to menu "Project" ---> "Project Inspector" --->  [Add] New Requirement ---> "tfpnoguigraphicsbridge_pack"
	c) Go to menu "Run" ---> "Build"

	2.2 TESTING:

	Open demo "AppTFPNoGUIGraphicsBridgeDemoX",  where X=1 or  X=2  or ....

		NOTE: LAMW provide some [automatic] support to TFPNoGuiGraphicsBridge component ...	

	a) Connect your device-usb  <--->  pc-usb
	b) Go to "Run" ---> "[Lamw] Build Android Apk and Run"

	CONGRATULATIONS!

Thank You!