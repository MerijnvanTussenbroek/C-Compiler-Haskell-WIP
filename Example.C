
int main()
{
    int x = 5;
    int y = 7;

    int z = Double(x);

    y = 5 + Square(y);
    return 0;
}

int Double(int x)
{
    return 2 * x;
}

int Square(int y)
{
    return y * y;
}