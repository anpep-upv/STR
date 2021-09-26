with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Practica_1 is
--
-- Exception declarations
--
   Circular_Buffer_Empty_Exception: exception;
   Circular_Buffer_Full_Exception: exception;

--
-- Circular buffer declarations
--
   Circular_Buffer_Capacity: constant Integer := 5;

   type Circular_Buffer_Index is mod Circular_Buffer_Capacity;
   type Circular_Buffer_Length is range 0..Circular_Buffer_Capacity;
   type Circular_Buffer_Data is array(Circular_Buffer_Index) of Integer;
   type Circular_Buffer is
   record
      Queue: Circular_Buffer_Data;
      Add_Index, Remove_Index: Circular_Buffer_Index := 0;
      Counter: Integer := 0;
   end record;

-- Initialize internal circular buffer state.
   procedure Circular_Buffer_Initialize(
      Circular_Buffer_Instance: in out Circular_Buffer
   ) is
   begin
      Circular_Buffer_Instance.Queue := (others => 0);
      Circular_Buffer_Instance.Add_Index := 0;
      Circular_Buffer_Instance.Remove_Index := 0;
      Circular_Buffer_Instance.Counter := 0;
   end Circular_Buffer_Initialize;

-- Add an Integer element to a circular buffer.
   procedure Circular_Buffer_Add(
      Circular_Buffer_Instance: in out Circular_Buffer;
      Element: in Integer
   ) is
   begin
      if Circular_Buffer_Instance.Counter = Circular_Buffer_Capacity
      then
         raise Circular_Buffer_Full_Exception;
      end if;

      Circular_Buffer_Instance.Queue(Circular_Buffer_Instance.Add_Index)
         := Element;

      Circular_Buffer_Instance.Add_Index
         := 1 + Circular_Buffer_Instance.Add_Index;

      Circular_Buffer_Instance.Counter
         := 1 + Circular_Buffer_Instance.Counter;
   end Circular_Buffer_Add;

-- Remove the last Integer element from a circular buffer.
   procedure Circular_Buffer_Remove(
      Circular_Buffer_Instance: in out Circular_Buffer;
      Element: out Integer
   ) is
   begin
      if Circular_Buffer_Instance.Counter = 0
      then
         raise Circular_Buffer_Empty_Exception;
      end if;

      Element :=
         Circular_Buffer_Instance.Queue(Circular_Buffer_Instance.Remove_Index);

      Circular_Buffer_Instance.Remove_Index :=
         1 + Circular_Buffer_Instance.Remove_Index;

      Circular_Buffer_Instance.Counter :=
         Circular_Buffer_Instance.Counter - 1;
   end Circular_Buffer_Remove;

-- Lists the contents of a circular buffer in order.
   procedure Circular_Buffer_List(
      Circular_Buffer_Instance: in out Circular_Buffer
   ) is
      Start_Index: Integer := Integer(Circular_Buffer_Instance.Remove_Index);
   begin
      Ada.Text_IO.Put("List has ");
      Ada.Integer_Text_IO.Put(Circular_Buffer_Instance.Counter);
      Ada.Text_IO.Put_Line(" element(s):");

      for Current_Index in Start_Index..(Start_Index + Circular_Buffer_Instance.Counter - 1)
      loop
         Ada.Text_IO.Put("Element ");
         Ada.Integer_Text_IO.Put(1 + Current_Index mod Circular_Buffer_Capacity);
         Ada.Integer_Text_IO.Put(Circular_Buffer_Instance.Queue(Circular_Buffer_Index(Current_Index rem Circular_Buffer_Capacity)));
         Ada.Text_IO.New_Line;
      end loop;
   end Circular_Buffer_List;

--
-- Program declarations
--
   Circular_Buffer_Instance: Circular_Buffer;
   Command, Element: Integer;
begin
   Ada.Text_IO.Put_Line("Circular Buffer Manager");
   
   loop
      Ada.Text_IO.Put_Line("1. Add element to buffer");
      Ada.Text_IO.Put_Line("2. Remove element from buffer");
      Ada.Text_IO.Put_Line("3. Initialize buffer");
      Ada.Text_IO.Put_Line("4. List buffer");
      Ada.Text_IO.Put_Line("0. Quit");
      
      Ada.Integer_Text_IO.Get(Command);
      case Command is
         when 1 =>
            begin
               Ada.Text_IO.Put("Element to add: ");
               Ada.Integer_Text_IO.Get(Element);
               Circular_Buffer_Add(Circular_Buffer_Instance, Element);
            exception
               when Circular_Buffer_Full_Exception =>
                  Ada.Text_IO.Put_Line("Error: Buffer is full");
            end;

         when 2 =>
            begin
               Circular_Buffer_Remove(Circular_Buffer_Instance, Element);
               Ada.Text_IO.Put("Last element removed: ");
               Ada.Integer_Text_IO.Put(Element);
               Ada.Text_IO.New_Line;
            exception
               when Circular_Buffer_Empty_Exception =>
                  Ada.Text_IO.Put_Line("Error: Buffer is empty");
            end;

         when 3 =>
            Circular_Buffer_Initialize(Circular_Buffer_Instance);

         when 4 =>
            Circular_Buffer_List(Circular_Buffer_Instance);

         when 0 =>
            exit;

         when others =>
            Ada.Text_IO.Put_Line("Please enter a valid option number");
      end case;
   end loop;
end Practica_1;