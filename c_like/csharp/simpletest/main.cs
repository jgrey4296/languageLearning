using System;

class MyTest {

    public MyTest(){
        Console.WriteLine("Constructed");
    }

    public int testAdd(int a){
        return a + 2;
    }    
}

class Hello{

    static private MyTest a = new MyTest();
    
    static void Main(){
        Console.WriteLine("hello world :" + a.testAdd(2));
        Func<int, int, bool> myLam = (x,y) => {
            return x == y;
        };
        Console.WriteLine("Lambda Test: " + myLam(2,3));

    }
    
}
