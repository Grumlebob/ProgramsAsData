.method public hidebysig static void  SelectionSort(int32[] arr) cil managed
{
  //allocate 0:int i, 1:int least, 2:int temp, 3:int j
  .locals init (int32 V_0, int32 V_1, int32 V_2, int32 V_3) 

  // int i = 0
  IL_0000:  ldc.i4.0      // Load 0 onto the stack
  IL_0001:  stloc.0       // Store the value on the stack in local variable V_0. V_0 is "int i"

  // Jump to the loop condition check. jumps to part that checks"i < arr.Length". 
  IL_0002:  br.s       IL_0032   // Branch to IL_0032 (conditional branch with short target) 

  // Loop through the array elements //   in code: int least = i and for (int j = i+1; j < arr.Length;..)
  IL_0004:  ldloc.0        // Load the value of V_0 (int i) onto the stack
  IL_0005:  stloc.1        // Store the value on the stack in local variable V_1  //   int least = i;
  IL_0006:  ldloc.0        // Load the value of V_0 onto the stack
  IL_0007:  ldc.i4.1       // Load constant 1 onto the stack
  IL_0008:  add            // Add the top two values on the stack
  IL_0009:  stloc.3        // Store the result in local variable V_3              // int j = i+1
  IL_000a:  br.s       IL_001a   // Branch to IL_001a (conditional branch with short target) //jumps to place that checks j < arr.Length

  // Inner loop to compare array elements //if (arr[j] < arr[least])
  IL_000c:  ldarg.0        // Load the argument (array) onto the stack
  IL_000d:  ldloc.3        // Load the value of V_3 onto the stack
  IL_000e:  ldelem.i4      // Load the element at index V_3 from the array onto the stack
  IL_000f:  ldarg.0        // Load the argument (array) onto the stack
  IL_0010:  ldloc.1        // Load the value of V_1 onto the stack
  IL_0011:  ldelem.i4      // Load the element at index V_1 from the array onto the stack
  IL_0012:  bge.s      IL_0016   // Branch to IL_0016 if the top two values on the stack are greater than or equal

  // Swap array elements if needed //least = j;
  IL_0014:  ldloc.3        // Load the value of V_3 onto the stack
  IL_0015:  stloc.1        // Store the value on the stack in local variable V_1

  // Increment the inner loop counter //j++
  IL_0016:  ldloc.3        // Load the value of V_3 onto the stack
  IL_0017:  ldc.i4.1       // Load constant 1 onto the stack
  IL_0018:  add            // Add the top two values on the stack
  IL_0019:  stloc.3        // Store the result in local variable V_3

  // Inner loop condition check // j < arr.Length
  IL_001a:  ldloc.3        // Load the value of V_3 onto the stack
  IL_001b:  ldarg.0        // Load the argument (array) onto the stack
  IL_001c:  ldlen          // Load the length of the array onto the stack
  IL_001d:  conv.i4        // Convert the top value on the stack to int32
  IL_001e:  blt.s      IL_000c   // Branch to IL_000c if the top two values on the stack are less than

  // Swap the minimum value with the current array element //int tmp = arr[i];
  IL_0020:  ldarg.0        // Load the argument (array) onto the stack
  IL_0021:  ldloc.0        // Load the value of V_0 onto the stack
  IL_0022:  ldelem.i4      // Load the element at index V_0 from the array onto the stack
  IL_0023:  stloc.2        // Store the value on the stack in local variable V_2 - In code: int tmp = arr[i];

  // Swap array elements //arr[i] = arr[least];
  IL_0024:  ldarg.0        // Load the argument (array) onto the stack
  IL_0025:  ldloc.0        // Load the value of V_0 onto the stack
  IL_0026:  ldarg.0        // Load the argument (array) onto the stack
  IL_0027:  ldloc.1        // Load the value of V_1 onto the stack
  IL_0028:  ldelem.i4      // Load the element at index V_1 from the array onto the stack
  IL_0029:  stelem.i4      // Store the top value on the stack in the element at index V_0 in the array

  // Continue swapping   //arr[least] = tmp;
  IL_002a:  ldarg.0        // Load the argument (array) onto the stack
  IL_002b:  ldloc.1        // Load the value of V_1 onto the stack
  IL_002c:  ldloc.2        // Load the value of V_2 onto the stack
  IL_002d:  stelem.i4      // Store the top value on the stack in the element at index V_1 in the array

  // Increment the outer loop counter //i++
  IL_002e:  ldloc.0        // Load the value of V_0 onto the stack
  IL_002f:  ldc.i4.1       // Load constant 1 onto the stack
  IL_0030:  add            // Add the top two values on the stack
  IL_0031:  stloc.0        // Store the result in local variable V_0

  // Outer loop condition check "i < arr.Length" If true, jump back to instruction number 4, else leave loop and return (instruction 38)
  IL_0032:  ldloc.0        // Load the value of V_0 onto the stack
  IL_0033:  ldarg.0        // Load the argument (array) onto the stack
  IL_0034:  ldlen          // Load the length of the array onto the stack
  IL_0035:  conv.i4        // Convert the top value on the stack to int32
  IL_0036:  blt.s      IL_0004   // Branch to IL_0004 if the top two values on the stack are less than

  // Return from the method
  IL_0038:  ret            // Return from the method
} // end of method Selsort::SelectionSort
