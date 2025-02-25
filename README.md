# xframes-freepascal

## Instructions

### Install Free Pascal

#### Ubuntu

`sudo apt install fpc`

##### x64 arch

I have had to make a 'tweak' to the logic to prevent 'Invalid floating point operation' to be thrown.

`rootNode.Add('id', 0.0);`

instead of

`rootNode.Add('id', 0);`

On Windows and Raspberry Pi `rootNode.Add('id', 0);` works just fine.

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

Windows 11

![image](https://github.com/user-attachments/assets/a962ec9b-03e0-4ff5-9335-f8e82f3cb4c6)

Ubuntu 24.04

![Screenshot from 2025-01-15 00-28-16](https://github.com/user-attachments/assets/8edaa163-ce4c-47fa-bd88-ce00fea050fa)

Raspberry Pi 5

![image](https://github.com/user-attachments/assets/d49764ab-b7a5-4f20-805d-bba23e3457ff)

