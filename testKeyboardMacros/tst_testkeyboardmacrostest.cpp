#include <QString>
#include <QtTest>
#include <QCoreApplication>
#include <QDebug>
#include <QStateMachine>

#include "KeyboardMacros.h"
#include "file2string.h"

class myeditor : public QTextEdit
{
    Q_OBJECT
public:

    myeditor(QWidget *p = 0) : QTextEdit(p) {
    }
    QPointer<KeyboardMacros> km;

protected:

    //KeyboardMacros km;

    virtual void keyPressEvent(QKeyEvent *e)
    {
        qDebug() << "keyPressEvent" << e;
        /*
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
        */
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
    myeditor ed[2];
    KeyboardMacros km;
    QEventLoop elp;

    auto init = [&](myeditor *ed, QString path) {
        ed->setWindowTitle(path);
        ed->setText(file2string(path));
        QShortcut *sc = new QShortcut(QKeySequence::Quit, ed);
        connect(sc, &QShortcut::activated, &elp, &QEventLoop::quit);
        ed->km = &km;
        ed->show();
    };
    init(ed+0, "/home/carlo/.bashrc");
    init(ed+1, "/home/carlo/.profile");

    elp.exec();

    QVERIFY2(true, "Success");
}

QTEST_MAIN(TestKeyboardMacrosTest)

#include "tst_testkeyboardmacrostest.moc"
