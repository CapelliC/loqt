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
        //connect(sc, &QShortcut::activatedAmbiguously, &elp, &QEventLoop::quit);

        QShortcut *start = new QShortcut(QKeySequence("Ctrl+Shift+R"), ed);
        QShortcut *stop = new QShortcut(QKeySequence("Ctrl+Shift+S"), ed);
        QShortcut *play = new QShortcut(QKeySequence("Ctrl+Shift+P"), ed);
        connect(start, &QShortcut::activated, ed, &myeditor::startRecording);
        connect(stop, &QShortcut::activated, ed, &myeditor::stopRecording);
        connect(play, &QShortcut::activated, ed, &myeditor::Playback);

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
