unit avrperipheral;

{$mode objfpc}{$H+}

interface

uses
  sysutils;

type
  { TBasePeripheral }
  TBasePeripheral = class
    // set default/startup values of registers, etc.
    procedure init; virtual;

    // Register list of IO registers - core to redirect IO to this peripheral using readIO/writeIO
    procedure registerIORegisters(out IOAddresses: TBytes); virtual;

    // Core to call this when writing to a register of this peripheral
    procedure writeIO(const IOAddress: word; const AValue: byte); virtual;

    // Core to call this when reading a register of this peripheral
    procedure readIO(const IOAddress: word; out AValue: byte); virtual;

    // Update peripheral with CPU clock tick count
    // Prescaling etc. should therefore happen inside peripheral
    // This means that peripheral clock dependent events may not be completely synchronous
    // because ticks will be updated after decoding and execution of the current instruction is completed
    procedure updateCPUClock(const ACPUTick: qword); virtual;
  end;

implementation

{ TBasePeripheral }

procedure TBasePeripheral.init;
begin

end;

procedure TBasePeripheral.registerIORegisters(out IOAddresses: TBytes);
begin

end;

procedure TBasePeripheral.writeIO(const IOAddress: word; const AValue: byte);
begin

end;

procedure TBasePeripheral.readIO(const IOAddress: word; out AValue: byte);
begin

end;

procedure TBasePeripheral.updateCPUClock(const ACPUTick: qword);
begin

end;

end.

