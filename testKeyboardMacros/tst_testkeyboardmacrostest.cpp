#include <QString>
#include <QtTest>
#include <QCoreApplication>

class TestKeyboardMacrosTest : public QObject
{
    Q_OBJECT

public:
    TestKeyboardMacrosTest();

private Q_SLOTS:
    void testCase1();
};

TestKeyboardMacrosTest::TestKeyboardMacrosTest()
{
}

void TestKeyboardMacrosTest::testCase1()
{
    QVERIFY2(false, "Failure");
}

QTEST_MAIN(TestKeyboardMacrosTest)

#include "tst_testkeyboardmacrostest.moc"
