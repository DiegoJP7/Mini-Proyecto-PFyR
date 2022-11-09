//Calculo Simpson 1/3

//Funcion
val f = (x: Double) => (-math.pow(x, 2) + 8 * x - 12)
f(3)
f(5)
def integral(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
integral(3, 5, f)

//Ejer2
val f1 = (x: Double) => (3 * math.pow(x, 2))
f1(0)
f1(2)
def integral1(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
integral(0, 2, f1)

//Ejer3
val f2 = (x: Double) => (x + (2 * math.pow(x, 2)) - (math.pow(x, 3)) + (math.pow(x, 4) * 5))
f2(-1)
f2(1)
def integral2(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
integral(-1, 1, f2)

//Ejer4
val f3 = (x: Double) => (((2 * x + 1) / (math.pow(x, 2) + x)))
f3(1)
f3(2)
def integral3(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
integral(1, 2, f3)

//ejer5
val f4 = (x: Double) => (math.E + math.pow(x, 2))
def integral4(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
integral(0, 1, f4)

//Ejer6
val f5 = (x: Double) => (1 / (math.sqrt(x - 1)))
def integral5(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
integral(2, 3, f5)

//Ejer7
val f6 = (x: Double) => (1 / (1 + math.pow(x, 2)))
def integral6(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
integral(0, 1, f6)


//Diferencias/errores

//Ejer1
def calcError(valorOb: Double, valorAbs: Double) = {
  (valorAbs - valorOb).abs
}
calcError(integral(3, 5, f), 7.33)
calcError(integral(0, 2, f1), 8.0)
calcError(integral(-1, 1, f2), 3.333)
calcError(integral(1, 2, f3), 1.09861)
calcError(integral(0, 1, f4), 1.71828)
calcError(integral(2, 3, f5), 0.828427)
calcError(integral(0, 1, f6), 0.785398)

