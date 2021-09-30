with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Exceptions;
with Buffers;

procedure Main is
   Buffer: Buffers.Circular_Buffer;
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
               Buffers.Circular_Buffer_Add(Buffer, Element);
            exception
               when E: Buffers.Circular_Buffer_Full_Exception =>
                  -- Ada.Text_IO.Put_Line("Error: Buffer is full");
                  Ada.Text_IO.Put_Line("Exception " & Ada.Exceptions.Exception_Name(E) & " in " & Ada.Exceptions.Exception_Message(E));
            end;

         when 2 =>
            begin
               Buffers.Circular_Buffer_Remove(Buffer, Element);
               Ada.Text_IO.Put("Last element removed: ");
               Ada.Integer_Text_IO.Put(Element);
               Ada.Text_IO.New_Line;
            exception
               when E: Buffers.Circular_Buffer_Empty_Exception =>
                  -- Ada.Text_IO.Put_Line("Error: Buffer is empty");
                  Ada.Text_IO.Put_Line("Exception " & Ada.Exceptions.Exception_Name(E) & " in " & Ada.Exceptions.Exception_Message(E));
            end;

         when 3 =>
            Buffers.Circular_Buffer_Initialize(Buffer);

         when 4 =>
            Buffers.Circular_Buffer_List(Buffer);

         when 0 =>
            exit;

         when others =>
            Ada.Text_IO.Put_Line("Please enter a valid option number");
      end case;
   end loop;
end Main;
