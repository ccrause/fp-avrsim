unit eeprom_periph;

interface

uses
  avrperipheral, avr, sysutils;

type
  // Simulate EEPROM - note EEPROM memory is stored in AVR object for easy programming
  // This peripheral just interfaces back to the core EEPROM memory block
  // Currently only implement atomic erase & write (no need for page buffer)
  { TEEPROM }
  TEEPROM = class(TBasePeripheral)
  private
    EEPROM: array of byte;
    EECR,
    EEDR,
    EEARL,
    EEARH: byte;
    fAVR: TAVR;
    // This is the timestamp when EEMPE is set.
    clockTicksWhenEEMPESet: qword;
    procedure setEECRRegister(const AValue: byte);
    function getSize: word;
  public
    constructor Create(const aAVR: TAVR; const AEEPROMSize: word);

    // Register list of IO registers - core to redirect IO to this peripheral using readIO/writeIO
    procedure registerIORegisters(out IOAddresses: TBytes); override;

    // Core to call this when writing to a register of this peripheral
    procedure writeIO(const IOAddress: word; const AValue: byte); override;

    // Core to call this when reading a register of this peripheral
    procedure readIO(const IOAddress: word; out AValue: byte); override;

    // Update peripheral with CPU clock tick count
    // Prescaling etc. should therefore happen inside peripheral
    // This means that peripheral clock dependent events may not be completely synchronous
    // because ticks will be updated after decoding and execution of the current instruction is completed
    procedure updateCPUClock(const ACPUTick: qword); override;
    property size: word read getSize;
  end;

implementation

const
  // Bit masks for registers
  // EECR
  EEPM1_mask = 1 shl 5;
  EEPM0_mask = 1 shl 4;
  EERIE_mask = 1 shl 3;
  EEMPE_mask = 1 shl 2;
  EEPE_mask = 1 shl 1;
  EERE_mask = 1;
  EECR_mask =  EEPM1_mask + EEPM0_mask + EERIE_mask + EEMPE_mask + EEPE_mask + EERE_mask;

  // Hardcoded for avrsim unit:
  EE_READY = 1;  // For atmega328P it is interrupt index 22

{ TEEPROM }

procedure TEEPROM.setEECRRegister(const AValue: byte);
begin
  // Read instruction inactive if EEPROM is busy programming
  // Strobe, so after read it should be cleared again??
  if ((AValue and EERE_mask) > 0) and
     ((EECR and EEPE_mask) = 0) then
  begin
    EEDR := EEPROM[(EEARH shl 8) or EEARL];
  end;

  // TODO: Write takes no clock cycles at the moment
  // EEPE can only be set within 4 clocks after setting EEMPE
  if ((AValue and EEPE_mask) > 0) and ((EECR and EEMPE_mask) > 0) then
  begin
    // If timing constraint is OK then write to EEPROM
    if ((fAVR.ClockTicks - clockTicksWhenEEMPESet) < 5) then
      EEPROM[(EEARH shl 8) or EEARL] := EEDR;

    // Clear EEMPE settings
    clockTicksWhenEEMPESet := 0;
    EECR := EECR and not(EEMPE_mask);
  end;

  if (AValue and EEMPE_mask) > 0 then
  begin
    EECR := EECR or EEMPE_mask;
    clockTicksWhenEEMPESet := fAVR.ClockTicks;
  end;

  if (AValue and EERIE_mask) > 0 then
    EECR := EECR or EERIE_mask;

  if (AValue and (EEPM1_mask or EEPM0_mask)) > 0 then
    writeln('Ignoring EEMP0/1 bits, only atomic write supported');
end;

function TEEPROM.getSize: word;
begin
  result := length(EEPROM);
end;

constructor TEEPROM.Create(const aAVR: TAVR; const AEEPROMSize: word);
begin
  fAVR := aAVR;
  SetLength(EEPROM, AEEPROMSize);
  // Fill EEPROM as if just deleted
  FillByte(EEPROM[0], AEEPROMSize, $FF);
  EECR  := 0;
  EEDR  := 0;
  EEARL := 0;
  EEARH := 0;
end;

procedure TEEPROM.registerIORegisters(out IOAddresses: TBytes);
begin
  SetLength(IOAddresses, 4);
  IOAddresses[0] := $3F;  // EECR
  IOAddresses[1] := $40;  // EEDR
  IOAddresses[2] := $41;  // EEARL
  IOAddresses[3] := $42;  // EEARH
end;

procedure TEEPROM.writeIO(const IOAddress: word; const AValue: byte);
begin
  case IOAddress of
    $3F: setEECRRegister(AValue);
    $40: EEDR  := AValue;
    // TODO: mask off overrange bits in address register(s)
    $41: EEARL := AValue;
    $42: EEARH := AValue;
  else
    writeln('Unsupported port write in EEPROM peripheral: $', HexStr(AValue, 2));
  end;
end;

procedure TEEPROM.readIO(const IOAddress: word; out AValue: byte);
begin
  case IOAddress of
    $3F: AValue := EECR and EECR_mask;
    $40: AValue := EEDR;
    // TODO: mask unmapped address bits, it should stay 0
    $41: AValue := EEARL;
    $42: AValue := EEARH;
  else
    writeln('Unsupported port read in EEPROM peripheral: $', HexStr(AValue, 2));
  end;
end;

procedure TEEPROM.updateCPUClock(const ACPUTick: qword);
begin
  if ((EECR and EERIE_mask) > 0) and ((EECR and EEPE_mask) = 0) then
    fAVR.queueInterrupt(EE_READY);
end;

end.

