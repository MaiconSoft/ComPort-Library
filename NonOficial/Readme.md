
# CPort non oficial package
This component package was written based on the CPort package created by Dejan Crnila. This components provides additional features such as:
  * Manipulate data in byte form, abstracting low level functions;
  * Filter data for packet preprocessing;
  * Accelerate code implementation using the serial port;

## Units

### CPort.Types
Concentrates the types created for data manipulation.

**TComDataBuffer**: Is a list of bytes supported byte access in the form of more complex variables:

* **AsWordIndex : Integer]**: It is a property that concatenates two bytes to form a WORD variable, in the same way, is possible to receive a WORD and convert it to two bytes, stored in position **Index** and **Index+1**.

* **AsDWordIndex : Integer]**: It is a property that concatenates four bytes to form a DWORD variable, likewise it is possible to receive a DWORD and convert it to four bytes, stored in the position. **Index** , **Index+1**, **Index+2** and **Index+3**.

* ** Function Read(Index: Integer; const Buffer; const Size: Integer)**: This function makes it possible to read the bytes in * records * facilitating the manipulation of the data. See **FixBigEndian**. Returns the number of bytes readed.

* ** Function Write(Index: Integer; const Buffer; const Size: Integer)**: This function makes it possible to write * records * in byte format facilitating the storage and transmission of data. See ** FixBigEndian **. Returns the number of bytes written.

* **Procedure FixBigEndian(const Buffer; Sizes: Array of byte)**: This function adds compatibility between LittleEndian BigEndian systems. Systems based on the Arm, PIC, Atmega microcontroller family, among others, work with a byte arrangement in variables formed by 2 more bytes, where the most significant byte is stored first, which is the same way we humans think, in however, current computers use a counter organization, the least significant bytes are stored first. This causes a problem when data is transmitted from microcontrollers (bigEndian) to computers (littleEndian), the bytes are inverted in these variables. This problem is primarily seen when data is received in the form * struct * (* record *). See mode [here](https://en.wikipedia.org/wiki/Endianness "here"). 
For use this function, pass * record * and a list of the bytes of each variable contained in * record *:

```pascal
TPack=packet record // Record for receve data, "packet" directive is need
	ParamA: byte;
	ParamB: word;
end;
...
var
	Pack: TPack;
```
```Pascal
procedure MyFunction();
Begin
	ComDataBuffer.read( 0, Pack, SizeOf(Pack));
	// Convert to LittleEndian
	FixBigEndian(Pack, [1, 2]); // Fix using 1 byte + 2bytes variables
	// Use bytes for what you want
	...
	Pack.ParamB := Pack.ParamB + 255;
	...
	// Convert back BigEndian
	FixBigEndian(Pack, [1, 2]); 
	ComDataBuffer.write( 0, Pack, SizeOf(Pack));
End;
```
* **ToHexString: String**:  Convert all bytes in the list to a formatted String, turn it printing easier. This property also allows a formatted String to be entered to fill the list with bytes. Note:
* Data is separated by space;
* Case insensitive;
* Invalid values will be ignored;
* Values with more than 8 bits (0x00 .. 0xFF) will be truncated.

* **As7bits**: Is a object of class * T7bByte *, the use if for manipulation of data with 7bits, as the protocols [MIDI](https://www.midi.org/specifications/item/table-1-summary-of-midi-message "MIDI")  and [Formata](https://github.com/firmata/protocol "Formata").

**TAs7bits**
* **As7bByte[Index: Integer]**: Lê e escreve bytes de 7 bits;
* **AsByte[Index: Integer]**: Lê e escreve bytes de 8 bits, organizados em dois bytes de 7 bits;
*8 bits byte =  **0xDE** [<span style="color:red;">1</span><span style="color:blue;">101 1110</span>b]*  converte em:
*7 bits bytes =  **0x01**[<span style="color:red;">0000 0001</span>b] **0x5E** [<span style="color:blue;">0101 1110</span>b]*.

* **AsWord[Index: Integer]**: Read and write 16-bit Word, arranged in three 7-bit bytes;
*16 bits word =  **0xDE85**  [<span style="color:red;">11</span><span style="color:blue;">01 1110 1</span><span style="color:orange;">000 0101</span>b]*  converte em: 
*7 bits bytes =  **0x03** [<span style="color:red;">0000 0011</span>b] **0x3D** [<span style="color:blue;">011 1101</span>b] **0x05** [<span style="color:orange;">0000 0101</span>b]] *.

* **AsNible**: Nible is a property of type T7bNible, which separates a byte into two parts, * low * and * high * of 4 bits. Although low and high variables allow values up to 8 bits,  if values above 15 is assign, they will be truncated.
*8 bits byte =  **0xDE** [<span style="color:red;">1101</span><span style="color:blue;"> 1110</span>b]*  converte em:
*4 bits bytes =  **high** : **0x0D** [<span style="color:red;">0000 1101 </span>b] and **low** : **0x0E** [<span style="color:blue;">0000 1110</span>b]* 

* ** AsBit [Index, bIndex: Integer] **: Reads and writes (set or clear) the bit of position ** bIndex ** of the list byte at position ** Index **.

* ** AsGeneric [Index, bIndex, bSize: Integer] **: Reads and writes a part of the list byte at position ** Index **, starting at position ** bIndex ** and size ** bSize **.
Example:
Set the bits 3 and 2:
Before: ** 0x58 ** [<span style = "color: red;"> 0101 </span> <span style = "color: blue;"> 10 </span> <span style = "color: red;" > 00 </span> b] >> After: ** 0x5C ** [<span style = "color: red;"> 0101 </span> <span style = "color: blue;"> 11 </span> <span style = "color: red;"> 00 </span> b]
```pascal
ComDataBuffer.Items [0]: = $ 58;
ComDataBuffer.As7bits.AsGeneric [0, 3, 2]: = $ 03; // 0x03 = 0000 0011b
```

###CPort.Data
This library is compost of TComDataBytePacket object.

**TComDataBytePacket** is a component based in TComDataPacket from CPort.pas, but it work with bytes, not chars, allowing receve data in protocols without readables chars.  This component suport a four types of protocol and a user defined protocol.

* **IncludeStrings**: After complete a packet of data, if this property is true, add StartString at begin and StopString at end of buffer. Otherwise only internal data was be processed. Default is *false*;

* **StartString**: is the begin tag allways start the packet, data was be only acept after a *StartString* tag and before *StopString* tag;

* **OnPacket**: is a event calling on packet is complete receved.

* **OnDiscard**: is a event calling at start receve a packet and on overflow buffer. 

* **Protocol**: select a type of protocol suported:
	* **cptlCustom**: is a user defined protocol, may used in decendent components, by override AddData function, otherwise no packet is build.

	* **cptlLine**: is a ReadLine protocol, each line ended with CR (0x13) and LF (0x10), make up a new packet;
	Exemple with **IncludeStrings** as false.
> Incame data:<span style = "color: blue;">  0x01 0x4F </span> <span style = "color: red;"> 0x13 0x10 </span><span style = "color: orange;">  0x0C 0xAA 0x66</span> <span style = "color: red;"> 0x13 0x10 </span>
> Packet 1: <span style = "color: blue;">0x01 0x4F </span>
> Packet 2: <span style = "color: orange;">0x0C 0xAA 0x66</span>

	* **cptlStartEnd**: is a Tagged data, only data between StartString and StopString, make up a new packet. Otherwise will be discard.
	Exemple with <span style = "color: red;">StartString = 0x00 0x00</span>, <span style = "color: purple;">  StopString = 0xFF 0xFF</span> and **IncludeStrings** as false.
> Incame data: <span style = "color: gray;">  0x01</span><span style = "color: red;">  0x00 0x00</span><span style = "color: blue;">  0x01 0x00 0xFF 0x00</span><span style = "color: purple;">  0xFF 0xFF</span><span style = "color: gray;">  0xFF</span>
> Discard 1: <span style = "color: gray;">  0x01</span>
> Packet 1: <span style = "color: blue;"> 0x01 0x00 0xFF 0x00 </span>
> Discard 2: <span style = "color: gray;">  0xFF</span>

	* **cptlStartSize**: in this protocol, the StartString indicate the begin of protocol, but the byte in position **FixedPosition** (defined by user) store the size of packet;
Exemple with **FixedPosition** = 2 and <span style = "color: red;">StartString = 0x00 0x00</span>
> Incame data:
<span style = "color: gray;">  0x01</span><span style = "color: red;">  0x00 0x00</span><span style = "color: blue;">  0xAA 0x33 </span><span style = "color: orange;">  0x05 </span><span style = "color: blue;">  0x80 0xDE</span><span style = "color: gray;">  0xAC 0xFF</span>
> Byte [2] = <span style = "color: orange;">  0x05 </span>indicate size of packet is five bytes;
> Discard 1: <span style = "color: gray;">  0x01</span>
> Packet 1: <span style = "color: blue;">0xAA 0x33 0x05 0x80 0xDE </span>
> Discard 2: <span style = "color: gray;">  0xAC 0xFF</span> 

	* **cptlStartFixedSize**:  in this protocol, the StartString indicate the begin of protocol, but the packet close after read **FixedSize** (defined by user)  bytes.
Exemple with **FixedSize** = 4 and <span style = "color: red;">StartString = 0x00 0x00</span>
> Incame data:
<span style = "color: gray;">  0x01</span><span style = "color: red;">  0x00 0x00</span><span style = "color: blue;">  0xAA 0x33 0x05 0x80 </span><span style = "color: gray;">0xDE 0xAC 0xFF</span>
> Discard 1: <span style = "color: gray;">  0x01</span>
> Packet 1: <span style = "color: blue;">0xAA 0x33 0x05 0x80 </span>
> Discard 2: <span style = "color: gray;">  0xDE 0xAC 0xFF</span> 


