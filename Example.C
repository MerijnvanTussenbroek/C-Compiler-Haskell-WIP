
typedef unsigned int testing;

enum test
{
    HIGH = 0,
    MID = 1,
    LOW = 5
};

enum test2
{
    ME,
    TEST,
    MYSELF = 3,
    I = 4
};


int main()
{
    enum test2 var;

    testing v;

    var = TEST;
    var = I;

    return 1;
}