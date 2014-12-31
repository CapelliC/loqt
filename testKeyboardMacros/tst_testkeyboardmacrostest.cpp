#include <QString>
#include <QtTest>
#include <QCoreApplication>
#include <QDebug>
#include <QStateMachine>

#include "KeyboardMacros.h"
#include "file2string.h"

/*
class myeditor : public QTextEdit
{
    Q_OBJECT
public:

    myeditor(QWidget *p = 0) : QTextEdit(p), capture(false) {
    }
    QPointer<KeyboardMacros> km;

public slots:

    void startRecording();
    void stopRecording();
    void Playback();

protected:

    bool capture;

    virtual void keyPressEvent(QKeyEvent *e)
    {
        qDebug() << "keyPressEvent" << e;
        if (capture)
            km->storeEvent(e);
        QTextEdit::keyPressEvent(e);
    }
};

void myeditor::startRecording() {
    km->startRecording(EditInterface(this));
    capture = true;
}
void myeditor::stopRecording() {
    if (capture) {
        km->doneRecording(EditInterface(this));
        capture= false;
    }
}
void myeditor::Playback() {
    if (!capture)
        km->startPlayback(EditInterface(this));
}
typedef QTextEdit myeditor;
*/

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
    QTextEdit ed[2];
    KeyboardMacros km;
    QEventLoop elp;

    auto init = [&](QTextEdit *ed, QString path) {
        ed->setWindowTitle(path);
        ed->setText(file2string(path));
        QShortcut *sc = new QShortcut(QKeySequence::Quit, ed);
        connect(sc, &QShortcut::activated, &elp, &QEventLoop::quit);

        //connect(sc, &QShortcut::activatedAmbiguously, &elp, &QEventLoop::quit);

        /*
        connect(km.start_(ed), &QShortcut::activated, ed, &myeditor::startRecording);
        connect(km.stop_(ed), &QShortcut::activated, ed, &myeditor::stopRecording);
        connect(km.play_(ed), &QShortcut::activated, ed, &myeditor::Playback);
        ed->km = &km;
        */

        /*
        km.connect(km.start_(ed), &QShortcut::activated, &myeditor::startRecording);
        km.connect(km.stop_(ed), &QShortcut::activated, &myeditor::stopRecording);
        km.connect(km.play_(ed), &QShortcut::activated, &myeditor::Playback);
        */

        ed->show();
    };
    init(ed+0, "/home/carlo/.bashrc");
    init(ed+1, "/home/carlo/.profile");

    elp.exec();

    QVERIFY2(true, "Success");
}

QTEST_MAIN(TestKeyboardMacrosTest)

#include "tst_testkeyboardmacrostest.moc"
