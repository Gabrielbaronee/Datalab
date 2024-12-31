/*
 * CS:APP Data Lab
 *
 * <Please put your name and userid here>
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:

  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code
  must conform to the following style:

  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>

  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.


  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31.


EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 *
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce
 *      the correct answers.
 */


#endif
//1
/* 
 * bitOr - x|y using only ~ and & 
 *   Example: bitOr(6, 5) = 7
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitOr(int x, int y) {
  return ~(~x & ~y);
}
/*
 * bitParity - returns 1 if x contains an odd number of 0's
 *   Examples: bitParity(5) = 0, bitParity(7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int bitParity(int x) {
  int resultado;
  x = x ^ x>>16;
  x = x ^ x>>8;
  x = x ^ x>>4;
  x = x ^ x>>2;
  x = x ^ x>>1;
  resultado = x & 1;
  return resultado;
}
/* 
 * bitNor - ~(x|y) using only ~ and & 
 *   Example: bitNor(0x6, 0x5) = 0xFFFFFFF8
 *   Legal ops: ~ &
 *   Max ops: 8
 *   Rating: 1
 */
int bitNor(int x, int y) {
  return (~x & ~y);
}
/* 
 * bitXor - x^y using only ~ and & 
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
  int w;
  int z;
    w = (x & ~y);

    z = (~x & y);

  return ~(~w & ~z);
}
//2
/* 
 * evenBits - return word with all even-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
int evenBits(void) {
  int word = 0x55;
  word = word << 8 | 0x55;
  word = word << 8 | 0x55;
  word = word << 8 | 0x55;

  return word;
}
/* 
 * anyOddBit - return 1 if any odd-numbered bit in word set to 1
 *   where bits are numbered from 0 (least significant) to 31 (most significant)
 *   Examples anyOddBit(0x5) = 0, anyOddBit(0x7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int anyOddBit(int x) {
  int mascara = 0xAA;
  int inpar;
  mascara = mascara << 8 | 0xAA;
  mascara = mascara << 8 | 0xAA;
  mascara = mascara << 8 | 0xAA;
  inpar = mascara & x;

  return !!inpar;
}
/* 
 * byteSwap - swaps the nth byte and the mth byte
 *  Examples: byteSwap(0x12345678, 1, 3) = 0x56341278
 *            byteSwap(0xDEADBEEF, 0, 2) = 0xDEEFBEAD
 *  You may assume that 0 <= n <= 3, 0 <= m <= 3
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 2
 */
int byteSwap(int x, int n, int m) {
  int byteN = (x >> (n<<3)) & 0xFF; //tomo el byte 0x56
  int byteM = (x >> (m<<3)) & 0xFF; //tomo el byte 0x12

  int mascaraN = ~(0xFF << (n<<3)); //0x0000FF00
  int mascaraM = ~(0XFF << (m<<3)); //0xFF000000

  x = x & mascaraN; //0x12340078
  x = x & mascaraM; //0x00340078
  x = x | (byteN << (m << 3)) | (byteM << (n << 3)); //0x56341278

  return x;
}
/* 
 * fitsBits - return 1 if x can be represented as an 
 *  n-bit, two's complement integer.
 *   1 <= n <= 32
 *   Examples: fitsBits(5,3) = 0, fitsBits(-4,3) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
int fitsBits(int x, int n) {
  //este calculo asegura el caso borde (0x80000000, 32), ya que calcular si x cumple -2**(n-1) <= x <= 2**(n-1) - 1 
  //falla el caso borde
  int x_movido;
  x_movido = x << (~n + 33);  //uso ~n + 33 por no poder usar el signo - (32 - n)
  x_movido = x_movido >> (~n + 33);

  return !(x ^ x_movido);
}
/* 
 * oddBits - return word with all odd-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int oddBits(void) {
  int word = 0xAA;
  word = word << 8 | 0xAA;
  word = word << 8 | 0xAA;
  word = word << 8 | 0xAA;

  return word;
}
/* 
 * sign - return 1 if positive, 0 if zero, and -1 if negative
 *  Examples: sign(130) = 1
 *            sign(-23) = -1
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 10
 *  Rating: 2
 */
int sign(int x){
  int negativo = x >> 31; // si es negativo, el MSB sera 1.

  int es_cero = !!x; //convierte cualquiern numero distinto de cero en 1, y si es cero devuelve cero.

  int respuesta = negativo | es_cero;
  return respuesta;
}
//3
/* 
 * addOK - Determine if can compute x+y without overflow
 *   Example: addOK(0x80000000,0x80000000) = 0,
 *            addOK(0x80000000,0x70000000) = 1, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int addOK(int x, int y) {
  int suma;
  int signo_x;
  int signo_y;
  int signo_suma;
  suma = x + y;
  signo_x = x >> 31; //signo mas significativo de x
  signo_y = y >> 31; //signo mas significativo de y
  signo_suma = suma >> 31; //signo mas significativo de la suma entre x e y
  
  return !(~(signo_x ^ signo_y) & (signo_suma ^ signo_y)); //verifico entre los signos si se cumple la condicion de overflow
}
/* 
 * bitMask - Generate a mask consisting of all 1's 
 *   lowbit and highbit
 *   Examples: bitMask(5,3) = 0x38
 *   Assume 0 <= lowbit <= 31, and 0 <= highbit <= 31
 *   If lowbit > highbit, then mask should be all 0's
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int bitMask(int highbit, int lowbit) {

  int mascara_unos = ~0x0;
  int mascara1 = ((1 << highbit << 1) + mascara_unos);
  int mascara2 = mascara1 << lowbit;
  
  return mascara1 & mascara2;
}
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int conditional(int x, int y, int z) {
  int booleano;
  booleano = !!x;
  booleano = ~booleano+1;
  return (booleano & y) | (~booleano & z);
}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {
  int mascara = 0x1;  
  int suma1 = 0;
  int suma2 = 0;
  
  mascara = mascara | (0x1 << 8);           
  mascara = mascara | (0x1 << 16);      
  mascara = mascara | (0x1 << 24);      

  
  suma1 = (x & mascara) + ((x >> 1) & mascara) + ((x >> 2) & mascara) + ((x >> 3) & mascara) + ((x >> 4) & mascara) + 
  ((x >> 5) & mascara) + ((x >> 6) & mascara) + ((x >> 7) & mascara);
  
  suma2 = (suma1 & 0xF) + ((suma1 >> 8) & 0xF) + ((suma1 >> 16) & 0xF) + ((suma1 >> 24) & 0xF);

  return suma2;
}
/* 
 * bitMatch - Create mask indicating which bits in x match those in y
 *            using only ~ and & 
 *   Example: bitMatch(0x7, 0xE) = 0x6
 *   Legal ops: ~ & |
 *   Max ops: 14
 *   Rating: 1
 */
int bitMatch(int x, int y) {
  return (x & y) | (~x & ~y);
}
/* 
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c) {
  int mascara;
  mascara = 0xFF; //mascara de todos 1's
  mascara = ~(mascara << (n << 3)); //mascara tiene 0's donde queremos reemplazar el byte

  c = c << (n << 3); //desplazo c en la posicion del byte deseado
  x = x & mascara; //uso la mascara con x para poder colocar el nuevo byte

  return x | c;
}
//4
/*
 * satAdd - adds two numbers but when positive overflow occurs, returns
 *          maximum possible value, and when negative overflow occurs,
 *          it returns minimum negative value.
 *   Examples: satAdd(0x40000000,0x40000000) = 0x7fffffff
 *             satAdd(0x80000000,0xffffffff) = 0x80000000
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 30
 *   Rating: 4
 */
int satAdd(int x, int y) {
  int suma;
  int signo_x;
  int signo_y;
  int signo_suma;
  int mascara_hay_overflow;
  int numero_minimo;
  int suma_es_negativa;
  suma = x + y;
  signo_x = x >> 31; // signo mas significativo de x
  signo_y = y >> 31; // signo mas significativo de y
  signo_suma = suma >> 31; // signo mas significativo de la suma
  mascara_hay_overflow = (~(signo_x ^ signo_y) & (signo_suma ^ signo_y)) >> 31; // chequeo si hay overflow
  //armo mascara, si hay overflow: mascara = 0, si no hay overflow mascara = 0XFFFFFFF

  
  numero_minimo = 1 << 31; // numero minimo posible = 0x8000000
  suma_es_negativa = suma >> 31; // si la suma es negativa = 1

  return (~mascara_hay_overflow & suma) | (mascara_hay_overflow & (numero_minimo ^ suma_es_negativa)); 
}
/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x80000001) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {

  int x_multiplicado = x << 1; // al mover el bit se multiplica por dos, ej: 0x30000000 -> 0x60000000
  int numero_minimo = 1 << 31; // minimo numero posible -> ej: 0x80000000
  //int numero_maximo = ~numero_minimo; // maximo numero posible
  int signo_multiplicacion = x_multiplicado >> 31; // signo de x multiplicado -> ej: 0x60000000 -> signo: 0
  int hubo_overflow = (x ^ x_multiplicado) >> 31; // si no hay overflow: 0, si hay overflow: 1 -> ej: 0

  return (~hubo_overflow & x_multiplicado) | (hubo_overflow & (numero_minimo ^ signo_multiplicacion));
}
/* 
 * isNonZero - Check whether x is nonzero using
 *              the legal operators except !
 *   Examples: isNonZero(3) = 1, isNonZero(0) = 0
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 4 
 */
int isNonZero(int x) {
  return ((x | (~x + 1)) >> 31) & 1;
}
/* 
 * rotateRight - Rotate x to the right by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321,4) = 0x187654321
 *   Legal ops: ~ & ^ | + << >> !
 *   Max ops: 25
 *   Rating: 3 
 */
int rotateRight(int x, int n){
  int mover_derecha = (x >> n) & ((1 << (~n + 33)) + ~0);
  //desplazo lo bits de x en n posiciones
  //(1 << (~n + 33) + ~0) -> desplazo al 1 las (~n + 33) posiciones necesarias, haciendo mi mascara
  //despues al restarle 1, me queda la mascara con 1 en las primeras ~n+33 posiciones y ceros en las demas
  //despues la aplico el AND(&)

  int mover_izquierda = x << ((~n + 1) + 32); // muevo los bits perdidos de la derecha a la izquierda

  return mover_derecha | mover_izquierda;
}
//float
/* 
 * floatAbsVal - Return bit-level equivalent of absolute value of f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument..
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatAbsVal(unsigned uf) {
  unsigned int mascara_bits;
  unsigned int uf_valor_absoluto;
  int condicion_NaN;
  mascara_bits = 0x7FFFFFFF; // seteo todos los bits en 1 excepto el bit mas significativo
  uf_valor_absoluto = uf & mascara_bits; // le aplico la mascara a uf para tener el valor absoluto
  condicion_NaN = uf_valor_absoluto > 0x7F800000; // verifico la condicion de NaN

  if(condicion_NaN == 1)
    return uf;
  
  return uf_valor_absoluto;
}
/* 
 * floatIsEqual - Compute f == g for floating point arguments f and g.
 *   Both the arguments are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   If either argument is NaN, return 0.
 *   +0 and -0 are considered equal.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 25
 *   Rating: 2
 */
int floatIsEqual(unsigned uf, unsigned ug) {
  unsigned caso_borde;
  if(((uf & 0x7FFFFFFF) > 0x7F800000) || ((ug & 0x7FFFFFFF) > 0x7F800000)) // analizo caso NaN
    return 0;

  caso_borde = ((uf | ug) << 1); 

  return (uf == ug) || caso_borde == 0;
}
/* 
 * floatNegate - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned floatNegate(unsigned uf) {
  int mascara_negadora;
 if((uf & 0x7FFFFFFF) > 0x7F800000) // analizo caso NaN
    return uf;

  mascara_negadora = 8 << 28; // 0x80000000
  return uf ^ mascara_negadora; 
}
/* 
 * floatIsLess - Compute f < g for floating point arguments f and g.
 *   Both the arguments are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   If either argument is NaN, return 0.
 *   +0 and -0 are considered equal.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 3
 */
int floatIsLess(unsigned uf, unsigned ug) {
  unsigned caso_borde;
  int signo_uf;
  int signo_ug;
  if(((uf & 0x7FFFFFFF) > 0x7F800000) || ((ug & 0x7FFFFFFF) > 0x7F800000)) // Analizo caso NaN
    return 0;
  
  caso_borde = ((uf | ug) << 1); // Analizo caso borde

  if(caso_borde == 0)
    return 0;

  signo_uf = uf >> 31; // tomo el bit mas significativo de uf
  signo_ug = ug >> 31; // tomo el bit mas significativo de ug

  if(signo_uf > signo_ug || ((signo_ug == 1 && ug < uf) || (signo_ug == 0 && uf < ug)))
    return 1;

  return 0; 
}
/* 
 * floatFloat2Int - Return bit-level equivalent of expression (int) f
 *   for floating point argument f.
 *   Argument is passed as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point value.
 *   Anything out of range (including NaN and infinity) should return
 *   0x80000000u.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
int floatFloat2Int(unsigned uf) {
  int bias;
  int exp;
  int signo;
  int E;
  int mascara_frac;
  int resultado;
  if((uf & 0x7FFFFFFF) > 0x7F800000) // analizo caso NaN
    return 0x80000000;

  // S bit de signo 1 bit, exp 8 bits, frac 23 bits
  
  bias = 127; // bias por defecto de 32 bits
  
  exp = (uf >> 23) & 0xFF; // el exp lo representan los 8 bits despues del bms
  
  signo = uf >> 31; // signo S de uf, sirve para decidir si el resultado es positivo o negativo
  
  E = exp - bias;
  if(E > 31) // si E es mayor a 31, no se puede representar en 32 bits
    return 0x80000000;
  if(E < 0) // si E es menor a 0, el resultado sera 0
    return 0;

  mascara_frac = (uf & 0x007FFFFF) | 0X00800000; // aislo la fraccion

  if(E > 23){
    resultado = mascara_frac << (E - 23);
  }
  else{
    resultado = mascara_frac >> (23 - E);
  }

  if(signo == 1){
    resultado = -resultado; // si el signo mas significativo es 1, el resultado sera negativo
  }

  return resultado;
}
/* 
 * floatPower2 - Return bit-level equivalent of the expression 2.0^x
 *   (2.0 raised to the power x) for any 32-bit integer x.
 *
 *   The unsigned value that is returned should have the identical bit
 *   representation as the single-precision floating-point number 2.0^x.
 *   If the result is too small to be represented as a denorm, return
 *   0. If too large, return +INF.
 * 
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. Also if, while 
 *   Max ops: 30 
 *   Rating: 4
 */
unsigned floatPower2(int x) {

    if(x > 127) // exponente muy grande
      return 0x7F800000;
    //rango de exponente -126 a 127
    if(x >= -126)
      return (x + 127) << 23;

    //rango de exponente valores desnormalizados
    if(x >= -149)
      return (1 << (x + 149));

    return 0;
}
