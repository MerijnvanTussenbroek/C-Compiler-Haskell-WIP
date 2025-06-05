
enum test
{
    HIGH,
    LOW
};

int main()
{
    int x = 5;
    x++;

    int y = x++;

    enum test x;

    y = ++x;

    y--;
}