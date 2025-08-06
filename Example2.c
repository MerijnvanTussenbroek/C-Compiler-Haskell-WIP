

int main()
{

    int test = 5;

    int result = factorial(test);

    return 1;
}

int factorial(int x)
{
    
    if(x == 0)
    {
        return 1;
    }
    else
    {
        return x * factorial(x-1);
    }
}