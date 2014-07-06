#include <QString>
#include <QtTest>
#include <QCoreApplication>
#include <QTextEdit>
#include <QDebug>

//#include <QStateMachine>
#include "KeyboardMacros.h"

class myeditor : public QTextEdit
{
    Q_OBJECT
public:
    myeditor(QWidget *p = 0) : QTextEdit(p) {}

signals:
    void quit();

protected:
    KeyboardMacros km;

    virtual void keyPressEvent(QKeyEvent *e)
    {
        qDebug() << "keyPressEvent" << e;
        if (e->matches(QKeySequence::Quit))
            emit quit();
        if (e->modifiers() == Qt::CTRL) {
            if (e->key() == 'R') {
                qDebug() << "km.startRecording(this)";
                km.startRecording(EditInterface(this));
            }
            else if (e->key() == 'S') {
                qDebug() << "km.doneRecording(this)";
                km.doneRecording(EditInterface(this));
            }
            else if (e->key() == 'P') {
                qDebug() << "km.startPlayback(this)";
                km.startPlayback(EditInterface(this));
            }
            else
                km.storeEvent(e);
        }
        else {
            km.storeEvent(e);
        }
        QTextEdit::keyPressEvent(e);
    }
};

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
    myeditor ed;
    ed.setText("aljsdla\nacz\n");
    ed.show();

    QEventLoop lp;
    connect(&ed, &myeditor::quit, &lp, &QEventLoop::quit);
    lp.exec();

    QVERIFY2(false, "Failure");
}

QTEST_MAIN(TestKeyboardMacrosTest)

#include "tst_testkeyboardmacrostest.moc"
