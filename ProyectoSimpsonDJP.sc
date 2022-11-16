//Calculo Simpson 1/3
println("                        Simpson 1/3")
//Funcion
def integral(a: Int, b: Int, f: Double => Double) = {
  val x = (a + b) / 2.0
  (b - a) * ((f(a) + (f(x) * 4) + f(b)) / 6)
}
//Ejer1
val f = (x: Double) => (-math.pow(x, 2) + 8 * x - 12)
integral(3, 5, f)
//Ejer2
val f1 = (x: Double) => (3 * math.pow(x, 2))
integral(0, 2, f1)
//Ejer3
val f2 = (x: Double) => (x + (2 * math.pow(x, 2)) - (math.pow(x, 3)) + (math.pow(x, 4) * 5))
integral(-1, 1, f2)
//Ejer4
val f3 = (x: Double) => (((2 * x + 1) / (math.pow(x, 2) + x)))
integral(1, 2, f3)
//ejer5
val f4 = (x: Double) => (math.pow(math.E, x))
integral(0, 1, f4)
//Ejer6
val f5 = (x: Double) => (1 / (math.sqrt(x - 1)))
integral(2, 3, f5)
//Ejer7
val f6 = (x: Double) => (1 / (1 + math.pow(x, 2)))
integral(0, 1, f6)



def calcError(valorOb: Double, valorAbs: Double) = {
  (valorOb-valorAbs).abs
}
println("                        Diferencias/errores")

println("Funcion 1:   "+calcError(integral(3, 5, f), 7.33))
println("Funcion 2:   "+calcError(integral(0, 2, f1), 8.0))
println("Funcion 3:   "+calcError(integral(-1, 1, f2), 3.333))
println("Funcion 4:   "+calcError(integral(1, 2, f3), 1.09861))
println("Funcion 5:   "+calcError(integral(0, 1, f4), 1.71828))
println("Funcion 6:   "+calcError(integral(2, 3, f5), 0.828427))
println("Funcion 7:   "+calcError(integral(0, 1, f6), 0.785398))

println("---------------------------------------------------------------------------------")
println("                        Simpson 1/3 Compuesta")
//Simpson 1/3 Compuesta
  def integralComp(a: Int, b: Int, n: Int, f: Double => Double): Double = {
    val h=(b-a)/(n*1.0)
    val xj =(j:Double)=>a+(j*h)
    val func = (j:Double)=> f(xj(2*j-2))+4*f(xj(2*j-1))+f(xj(2*j))
  //Agregacion de la aproximacion de la manera correcta
  //Rango  n/2
    (1 to n/2).map(func(_)).sum*(h/3)
}
println("                        Simpson 1/3 Compuesta Resultado")

println("Funcion 1:   "+integralComp(3,5,100,f))
println("Funcion 2:   "+integralComp(0,2,100,f1))
println("Funcion 3:   "+integralComp(-1,1,1000,f2))
println("Funcion 4:   "+integralComp(1, 2, 100,f3))
println("Funcion 5:   "+integralComp(0, 1,100, f4))
println("Funcion 6:   "+integralComp(2, 3,100, f5))
println("Funcion 7:   "+integralComp(0, 1,100, f6))

println("                        Simpson 1/3 Compuesta Calculo de error ")
//Calculo de error
println("Funcion 1:   "+calcError(integralComp(3,5,100,f), 7.33))
println("Funcion 2:   "+calcError(integralComp(0,2,100,f1), 8.0))
println("Funcion 3:   "+calcError(integralComp(-1,1,1000,f2), 3.333))
println("Funcion 4:   "+calcError(integralComp(1, 2, 100,f3), 1.09861))
println("Funcion 5:   "+calcError(integralComp(0, 1,100, f4), 1.71828))
println("Funcion 6:   "+calcError(integralComp(2, 3,100, f5), 0.828427))
println("Funcion 7:   "+calcError(integralComp(0, 1,100, f6), 0.785398))

println("---------------------------------------------------------------------------------")
//Simpson 1/3 Extendida
println("                       Simpson 1/3 Extendida")
def integralExt(a:Int, b:Int, f:Double=>Double) ={
  val i=1
  val j=2
  val n = 2 * (b - a)
  val h = (b - a) / (n * 1.0)
  //Correcion de error
  val functionj=(j:Double)=> f(a+(j*h))
  val functioni=(i:Double)=> (f(a+(i*h)))
  val fun= f(a)+(4* (1 until n by 2).map(functionj(_)).sum)+(2*(2 to n-2 by 2).map(functioni(_)).sum)+f(b)
  (h/3)*fun
}
println("                        Simpson 1/3 Extendida Resultado")
println("Funcion 1:   "+integralExt(3,5,f))
println("Funcion 2:   "+integralExt(0, 2, f1))
println("Funcion 3:   "+integralExt(-1,1,f2))
println("Funcion 4:   "+integralExt(1, 2,f3))
println("Funcion 5:   "+integralExt(0, 1,f4))
println("Funcion 6:   "+integralExt(2, 3,f5))
println("Funcion 7:   "+integralExt(0, 1,f6))

println("                        Calculo error Simpson 1/3 Extendida")
println("Funcion 1:   "+calcError(integralExt(3, 5, f), 7.33))
println("Funcion 2:   "+calcError(integralExt(0, 2, f1), 8.0))
println("Funcion 3:   "+calcError(integralExt(-1, 1, f2), 3.333))
println("Funcion 4:   "+calcError(integralExt(1, 2, f3), 1.09861))
println("Funcion 5:   "+calcError(integralExt(0, 1, f4), 1.71828))
println("Funcion 6:   "+calcError(integralExt(2, 3, f5), 0.828427))
println("Funcion 7:   "+calcError(integralExt(0, 1, f6), 0.785398))



