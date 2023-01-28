/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#include "KeyboardMacros.h"
#include <QCoreApplication>
#include <QDebug>

static const char* k_macros = "KeyboardMacros";
static const char* k_name = "name";
static const char* k_strokes = "strokes";

KeyboardMacros::KeyboardMacros(QObject *parent) :
    QObject(parent),
    mapStart(new QSignalMapper(this)),
    mapStop(new QSignalMapper(this)),
    mapPlay(new QSignalMapper(this)),
    status(idle)
{
    connect(mapStart, SIGNAL(mappedObject(QWidget*)), this, SLOT(startRecording(QWidget*)));
    connect(mapStop, SIGNAL(mappedObject(QWidget*)), this, SLOT(stopRecording(QWidget*)));
    connect(mapPlay, SIGNAL(mappedObject(QWidget*)), this, SLOT(Playback(QWidget*)));

    lqPreferences p;
#if 0
    int n = p.beginReadArray(k_macros);
    for (int i = 0; i < n; ++i) {
        p.setArrayIndex(i);
        QString name = p.value(k_name).toString();
        QStringList strokes = p.value(k_strokes).toStringList();
        macro m;
        foreach(QString s, strokes)
            m << s2e(s);
        macros[name] = m;
    }
#endif
    p.endArray();
}

KeyboardMacros::~KeyboardMacros()
{
#if 0
    lqPreferences p;

    p.beginWriteArray(k_macros);
    int i = 0;
    foreach(QString name, macros.keys())
        { //if (name != defaultName()) {
            p.setArrayIndex(i);
            p.setValue(k_name, name);

            QStringList s;
            foreach (const QKeyEvent &e, macros[name])
                s << e2s(e);
            p.setValue(k_strokes, s);
        }
    p.endArray();
#endif
}

QKeySequence KeyboardMacros::start() { return QKeySequence("Ctrl+Shift+R"); }
QKeySequence KeyboardMacros::stop() { return QKeySequence("Ctrl+Shift+T"); }
QKeySequence KeyboardMacros::play() { return QKeySequence("Ctrl+Shift+Y"); }

/**
 * @brief KeyboardMacros::manage
 *  add widget to managed: just hook action triggers
 * @param w
 *  widget to manage
 */
void KeyboardMacros::manage(QWidget *w)
{
    qDebug() << "manage" << w;

    if (macroStartRegAct) {
        mapStart->setMapping(macroStartRegAct, w);
        connect(macroStartRegAct, SIGNAL(triggered()), mapStart, SLOT(map()));
    } else {
        auto s = start_(w);
        mapStart->setMapping(s, w);
        connect(s, SIGNAL(activated()), mapStart, SLOT(map()));
    }

    if (macroStopRegAct) {
        mapStop->setMapping(macroStopRegAct, w);
        connect(macroStopRegAct, SIGNAL(triggered()), mapStop, SLOT(map()));
    } else {
        auto s = stop_(w);
        mapStop->setMapping(s, w);
        connect(s, SIGNAL(activated()), mapStop, SLOT(map()));
    }

    if (macroPlaybackAct) {
        mapPlay->setMapping(macroPlaybackAct, w);
        connect(macroPlaybackAct, SIGNAL(triggered()), mapPlay, SLOT(map()));
    } else {
        auto s = play_(w);
        mapPlay->setMapping(s, w);
        connect(s, SIGNAL(activated()), mapPlay, SLOT(map()));
    }
}

void KeyboardMacros::setupMenu(QMenu *menu)
{
    qDebug() << "setupMenu" << menu;

    macroStartRegAct = new QAction(tr("Macro Start Recording"), this);
    macroStartRegAct->setShortcut(start());
    macroStartRegAct->setStatusTip(tr("Keyboard Macro: Start Recording"));
    //connect(macroStartRegAct, SIGNAL(triggered()), this, SLOT(macroStartReg()));

    macroStopRegAct = new QAction(tr("Macro Stop Recording"), this);
    macroStopRegAct->setShortcut(stop());
    macroStopRegAct->setStatusTip(tr("Keyboard Macro: Stop Recording"));
    //connect(macroStopRegAct, SIGNAL(triggered()), this, SLOT(macroStopReg()));

    macroPlaybackAct = new QAction(tr("Macro Playback"), this);
    macroPlaybackAct->setShortcut(play());
    macroPlaybackAct->setStatusTip(tr("Keyboard Macro: Run Last Recorded"));
    //connect(macroPlaybackAct, SIGNAL(triggered()), this, SLOT(macroPlayback()));
    /*
    macroSelectAct = new QAction(tr("Macro Select"), this);
    macroSelectAct->setStatusTip(tr("Select and run a Keyboard Macro Registered"));
    connect(macroSelectAct, SIGNAL(triggered()), this, SLOT(macroSelect()));
    */

    menu->addSeparator();
    menu->addAction(macroStartRegAct);
    menu->addAction(macroStopRegAct);
    menu->addAction(macroPlaybackAct);
    //menu->addAction(macroSelectAct);
}

void KeyboardMacros::storeEvent(QKeyEvent *e)
{
    if (!lastRecorded.isEmpty()) {
        qDebug() << "storeEvent" << e;
#if 0
        macros[lastRecorded].append(*e);
#endif
    }
}
void KeyboardMacros::setLastRecordedName(QString name)
{
#if 0
    macros[name] = macros[lastRecorded];
    macros.remove(lastRecorded);
#endif
    lastRecorded = name;
}

QStringList KeyboardMacros::e2l(const QKeyEvent &e)
{
    QStringList l;
    l << QString::number(e.type()) << QString::number(e.key()) << QString::number(e.modifiers());
    return l;
}

QKeyEvent KeyboardMacros::l2e(const QStringList &l)
{
    int type = l[0].toInt(), key = l[1].toInt(), modif = l[2].toInt();
    return QKeyEvent(QEvent::Type(type), key, Qt::KeyboardModifier(modif));
}

bool KeyboardMacros::eventFilter(QObject *obj, QEvent *event)
{
    qDebug() << "KeyboardMacros::eventFilter" << obj << event;
    if (event->type() == QEvent::KeyPress) {
        QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
        storeEvent(keyEvent);
    }
    // standard event processing
    return QObject::eventFilter(obj, event);
}

void KeyboardMacros::startRecording(QWidget *ed)
{
    qDebug() << "startRecording" << status << ed;
    if (status == idle) {
        status = onRecord;
        lastRecorded = defaultName();
#if 0
        macros[lastRecorded].clear();
        ed->installEventFilter(this);
#endif
        emit feedback(tr("Keyboard Macro: Start registering '%1'").arg(currName()));
    }
    else
        emit feedback(tr("Keyboard Macro: Already registering unnamed "));
}

void KeyboardMacros::stopRecording(QWidget *ed)
{
    qDebug() << "stopRecording" << ed;
    if (status == onRecord) {
        ed->removeEventFilter(this);
        status = idle;
        emit feedback(tr("Keyboard Macro: Done recording '%1'").arg(currName()));
        emit registerCompleted();
    }
    else
        emit feedback(tr("Keyboard Macro: Not recording now"));
}

void KeyboardMacros::Playback(QWidget *ed)
{
    qDebug() << "Playback" << ed << status;

    // shortcut: play whatever recorded so far
    if (status == onRecord) {
        emit feedback(tr("Keyboard Macro: Stop recording '%1'").arg(currName()));
        ed->removeEventFilter(this);
        status = idle;
    }

    if (status == idle) {
        status = onPlayback;
        emit feedback(tr("Keyboard Macro: Playback '%1'").arg(currName()));
#if 0
        for (const QKeyEvent& e: macros[defaultName()])
            QCoreApplication::postEvent(ed, new QKeyEvent(e.type(), e.key(), e.modifiers(), e.text()));
#endif
        status = idle;
        emit playbackCompleted();
    }
    else
        emit feedback(tr("Keyboard Macro: Currently recording, cannot playback"));
}
