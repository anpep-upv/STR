with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body Buffers is
   procedure Circular_Buffer_Add(Buffer: in out Circular_Buffer;
                                 Element: in Integer) is
      
   begin
      if Buffer.Counter = Circular_Buffer_Capacity
      then
         raise Circular_Buffer_Full_Exception;
      end if;

      Buffer.Queue(Buffer.Add_Index)
         := Element;

      Buffer.Add_Index
         := 1 + Buffer.Add_Index;

      Buffer.Counter
         := 1 + Buffer.Counter;
   end Circular_Buffer_Add;
   
   procedure Circular_Buffer_Remove(Buffer: in out Circular_Buffer;
                                    Element: out Integer) is
   begin
      if Buffer.Counter = 0
      then
         raise Circular_Buffer_Empty_Exception;
      end if;

      Element :=
         Buffer.Queue(Buffer.Remove_Index);

      Buffer.Remove_Index :=
         1 + Buffer.Remove_Index;

      Buffer.Counter :=
         Buffer.Counter - 1;
   end Circular_Buffer_Remove;
   
   procedure Circular_Buffer_Initialize(Buffer: in out Circular_Buffer) is
   begin
      Buffer.Queue := (others => 0);
      Buffer.Add_Index := 0;
      Buffer.Remove_Index := 0;
      Buffer.Counter := 0;
   end Circular_Buffer_Initialize;
   
   procedure Circular_Buffer_List(Buffer: in Circular_Buffer) is
      Start_Index: Integer := Integer(Buffer.Remove_Index);
   begin
      Ada.Text_IO.Put("List has ");
      Ada.Integer_Text_IO.Put(Buffer.Counter);
      Ada.Text_IO.Put_Line(" element(s):");

      for Current_Index in Start_Index..(Start_Index + Buffer.Counter - 1)
      loop
         Ada.Text_IO.Put("Element ");
         Ada.Integer_Text_IO.Put(1 + Current_Index mod Circular_Buffer_Capacity);
         Ada.Integer_Text_IO.Put(Buffer.Queue(Circular_Buffer_Index(Current_Index rem Circular_Buffer_Capacity)));
         Ada.Text_IO.New_Line;
      end loop;
   end Circular_Buffer_List;
end Buffers;
