nasm -f bin -o rom.bin rom.asm
nasm -f bin -o truthMachine.bin truthMachine.asm
cd bin
make
cd ..
"./bin/IBMPcEmu.exe"