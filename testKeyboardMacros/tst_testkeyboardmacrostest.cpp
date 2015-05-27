/*
    testKeyboardMacros: developing LoQT macro recording and playback

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <QtTest>
#include <QDebug>
#include <QCoreApplication>

#include "KeyboardMacros.h"
#include "file2string.h"

class TestKeyboardMacrosTest : public QObject
{
    Q_OBJECT

private Q_SLOTS:

    void testCase1() {
        QTextEdit ed;
        init(&ed, "~/.bashrc");
        elp.exec();
        QVERIFY2(true, "Success");
    }

    void testCase2() {
        QTextEdit ed[2];
        init(ed+0, "~/.bashrc");
        init(ed+1, "~/.profile");
        elp.exec();
        QVERIFY2(true, "Success");
    }

private:

    void init(QTextEdit *ed, QString path) {
        ed->setWindowTitle(path);
        ed->setText(file2string(path));
        ed->connect(new QShortcut(QKeySequence("Ctrl+Q"), ed),
                    &QShortcut::activated, &elp, &QEventLoop::quit);
        ed->show();
        km.manage(ed);
    }

    KeyboardMacros km;
    QEventLoop elp;
};

QTEST_MAIN(TestKeyboardMacrosTest)

#include "tst_testkeyboardmacrostest.moc"
