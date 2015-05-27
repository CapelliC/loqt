/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
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
    connect(mapStart, SIGNAL(mapped(QWidget*)), this, SLOT(startRecording(QWidget*)));
    connect(mapStop, SIGNAL(mapped(QWidget*)), this, SLOT(stopRecording(QWidget*)));
    connect(mapPlay, SIGNAL(mapped(QWidget*)), this, SLOT(startPlayback(QWidget*)));

    lqPreferences p;
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
    p.endArray();
}

KeyboardMacros::~KeyboardMacros()
{
    lqPreferences p;

    p.beginWriteArray(k_macros);
    int i = 0;
    foreach(QString name, macros.keys())
        { //if (name != defaultName()) {
            p.setArrayIndex(i);
            p.setValue(k_name, name);
            QStringList s;
            foreach (QKeyEvent e, macros[name])
                s << e2s(e);
            p.setValue(k_strokes, s);
        }
    p.endArray();
}

QKeySequence KeyboardMacros::start() { return QKeySequence("Ctrl+Shift+R"); }
QKeySequence KeyboardMacros::stop() { return QKeySequence("Ctrl+Shift+T"); }
QKeySequence KeyboardMacros::play() { return QKeySequence("Ctrl+Shift+Y"); }

void KeyboardMacros::manage(QWidget *w)
{
    QShortcut *v[3] = { start_(w), stop_(w), play_(w) };

    mapStart->setMapping(v[0], w);
    connect(v[0], SIGNAL(activated()), mapStart, SLOT(map()));

    mapStop->setMapping(v[1], w);
    connect(v[1], SIGNAL(activated()), mapStop, SLOT(map()));

    mapPlay->setMapping(v[2], w);
    connect(v[2], SIGNAL(activated()), mapPlay, SLOT(map()));

    //managed << w;
    //w->installEventFilter(this);
}

void KeyboardMacros::setupGUI(QStatusBar *bar, QMenu *menu)
{
    this->menu = menu;
    this->statusBar = bar;
    // tbd
    qDebug() << "setupGUI(QStatusBar *bar, QMenu *menu)" << bar << menu;
}

void KeyboardMacros::storeEvent(QKeyEvent *e)
{
    if (!lastRecorded.isEmpty()) {
        qDebug() << "storeEvent" << e;
        macros[lastRecorded].append(*e);
    }
}
void KeyboardMacros::setLastRecordedName(QString name)
{
    macros[name] = macros[lastRecorded];
    macros.remove(lastRecorded);
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
        macros[lastRecorded].clear();
        ed->installEventFilter(this);
    }
}

void KeyboardMacros::stopRecording(QWidget *ed)
{
    qDebug() << "stopRecording" << ed;
    if (status == onRecord) {
        ed->removeEventFilter(this);
        status = idle;
    }
}

void KeyboardMacros::startPlayback(QWidget *ed)
{
    qDebug() << "startPlayback" << ed;
    if (status == idle) {
        status = onPlayback;
        for (auto e: macros[defaultName()])
            QCoreApplication::postEvent(ed, new QKeyEvent(e.type(), e.key(), e.modifiers(), e.text()));
        status = idle;
    }
}
