package Buffers is
   Circular_Buffer_Capacity: constant Integer;
   
   type Circular_Buffer is private;
   type Circular_Buffer_Index is private;
   type Circular_Buffer_Data is private;
   
   procedure Circular_Buffer_Add(Buffer: in out Circular_Buffer;
                                 Element: in Integer);
   procedure Circular_Buffer_Remove(Buffer: in out Circular_Buffer;
                                    Element: out Integer);
   procedure Circular_Buffer_Initialize(Buffer: in out Circular_Buffer);
   procedure Circular_Buffer_List(Buffer: in Circular_Buffer);
   
   Circular_Buffer_Empty_Exception: exception;
   Circular_Buffer_Full_Exception: exception;
   
private
   Circular_Buffer_Capacity: constant Integer := 5;
   
   type Circular_Buffer_Index is mod Circular_Buffer_Capacity;
   type Circular_Buffer_Data is array(Circular_Buffer_Index) of Integer;
   
   type Circular_Buffer is
      record
         Queue: Circular_Buffer_Data;
         Add_Index, Remove_Index: Circular_Buffer_Index := 0;
         Counter: Integer := 0;
      end record;
end Buffers;
