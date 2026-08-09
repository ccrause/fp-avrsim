# fp-avrsim
AVR simulator ported to FreePascal, with integrated GDB server.

## Compiling
**lazbuild avrsim.lpi**

or

**fpc avrsim.lpr**

## Usage
```
Usage: avrsim [-d<port>] [-6] [-v] [-s<startAddress>] <bin-file>
  -6         - Simulate an AVR6 device (18 bit PC). Defaults to 16 bit PC.
  -d<port>   - Start as GDB server and listen on port <port> for a remote connection.
  -h         - Print this help message then exit.
  -i<addr>   - Override I/O starting address. Defaults to 32.
  -s<addr>   - Override RAM starting address. Defaults to 256.
  -v         - Verbose output.
  <bin-file> - Binary image of firmware to load.
```
Flash, I/O and SRAM memories are stored and maintained in the host computer memory.
Some I/O registers are interpreted according to the avrsim controller definitions:
* 32 : OUTPUTREG, data written to this register is also written to terminal.
* 33 : EXITCODEREG, data written to this register is returned as exit code when the simulation ends.
* 34 : HALTREQUEST, writing a 1 to this register will exit the simulation.
* 35-38 : Cycle count of simulation, read as a 4 byte little endian value.
* EXCEPTIONJMPZERO, if set to non-zero value the simulation will raise an exception if a jmp or call instruction target address is 0.

### Simple simulation
```
avrsim file.bin
```
Normal simulation which loads file.bin and simulates the instructions.

### Load firmware and wait for remote debug connection
```
avrsim -d2159 file.bin
```
Starts a new simulation, loads file.bin to flash memory, halts at address 0, and waits for GDB Remote connections on the given port (2159 in this example).

### Start simulation and wait for remote debug connection
```
avrsim -d1234
```
Starts a new simulation, halts at address 0, and waits for a remote GDB connections on the given port (1234 in this example).
The debugger can then program the flash using the RSP _M_ command.

### Simulate as avr6 controller
```
avrsim -6 file.bin
```
Increases the width of PC to 22 bits to enable simulation of large memory devices. Useful for testing of complex code.
