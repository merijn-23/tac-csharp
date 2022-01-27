// comment 1
// comment 2
// comment 4
class Hello
{
    void main()
    {
        if(false)
        {
          print(funcOne(1, 2));
        }
        else
        {
          int a;
          a = funcTwo(2, 2);
          print(a);
        }
    }

    int funcOne(int a, int b)
    {
      return a + b;
    }

    int funcTwo(int a, int b)
    {
      return a * b;
    }


}
