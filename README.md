# xframes-freepascal

## Instructions

### Install Free Pascal

#### Ubuntu

sudo apt update
sudo apt install fpc

I have had to make a 'tweak' to the logic to prevent 'Invalid floating point operation' to be thrown.

`rootNode.Add('id', 0.0);`

instead of

`rootNode.Add('id', 0);`

On Windows `rootNode.Add('id', 0);` works just fine.

#### Windows

There's no native x64 compiler for Windows. You need to first download the 32 bit compiler from

https://www.freepascal.org/down/i386/win32.html

Then you need to install the cross compiler

https://www.freepascal.org/down/x86_64/win64.html

### Compiling the application

Compile for x64

`fpc -Px86_64 main.pas`

### Running the application

Run `main.exe`

## Screenshots

![image](https://github.com/user-attachments/assets/a962ec9b-03e0-4ff5-9335-f8e82f3cb4c6)

