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
#include "lqPreferences.h"

#include <QRegularExpression>
#include <QCoreApplication>
#include <QTextStream>
#include <QDebug>

static const char* k_macros = "KeyboardMacros";
static const char* k_name = "name";
static const char* k_strokes = "strokes";

KeyboardMacros::keystroke KeyboardMacros::e2k(const QKeyEvent &e) {
    keystroke k;
    k.type = e.type();
    k.key = e.key();
    k.modifiers = e.modifiers();
    k.text = e.text();
    k.autorep = e.isAutoRepeat();
    k.count = e.count();
    return k;
}
QKeyEvent KeyboardMacros::k2e(const KeyboardMacros::keystroke& k) {
    return QKeyEvent(
        k.type,
        k.key,
        k.modifiers,
        k.text,
        k.autorep,
        k.count);
}

KeyboardMacros::keystroke KeyboardMacros::s2k(const QString &s) {
    keystroke k;
    /*
    #define cI "(\\d+)"
    #define cS "([^,]*)"
    QRegularExpression e(cI "," cI "," cI "," cS "," cI "," cI);
    auto m = e.match(s);
    if (m.hasMatch()) {
        k.type      = static_cast<QKeyEvent::Type>(m.captured(1).toUInt());
        k.key       = m.captured(2).toInt();
        k.modifiers = static_cast<Qt::KeyboardModifiers>(m.captured(3).toUInt());
        k.text      = m.captured(4);
        k.autorep   = m.captured(5).toUInt();
        k.count     = m.captured(6).toUInt();
    }
    */
    QString t(s);
    QTextStream S(&t);
    quint32 type, modifiers, autorep;
    S >> type >> k.key >> modifiers >> k.text >> autorep >> k.count;
    k.type      = static_cast<QKeyEvent::Type>(type);
    k.modifiers = static_cast<Qt::KeyboardModifiers>(modifiers);
    k.autorep   = autorep;
    return k;
}
QString KeyboardMacros::k2s(const KeyboardMacros::keystroke& k) {
    QString s;
    {   QTextStream S(&s);
        S << k.type << ' ' << k.key << ' ' << k.modifiers << ' ' << k.text << ' ' << k.autorep << ' ' << k.count;
    }
    return s;
}

KeyboardMacros::KeyboardMacros(QObject *parent) :
    QObject(parent),
    status(idle)
{
    lqPreferences p;

    int n = p.beginReadArray(k_macros);
    for (int i = 0; i < n; ++i) {
        p.setArrayIndex(i);
        QString name = p.value(k_name).toString();
        QStringList strokes = p.value(k_strokes).toStringList();
        macro m;
        foreach(QString s, strokes)
            m << s2k(s);
        macros[name] = m;
    }

    p.endArray();
}

KeyboardMacros::~KeyboardMacros()
{
    lqPreferences p;

    p.beginWriteArray(k_macros);
    int i = 0;
    foreach(QString name, macros.keys()) {
        p.setArrayIndex(i);
        p.setValue(k_name, name);

        QStringList s;
        foreach (const keystroke &k, macros[name])
            s << k2s(k);

        p.setValue(k_strokes, s);
    }
    p.endArray();
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
        connect(macroStartRegAct, &QAction::triggered, [w, this]{ startRecording(w); });
    } else {
        Q_ASSERT(false);
        auto s = start_(w);
        connect(s, &QShortcut::activated, [w, this]{ startRecording(w); });
    }

    if (macroStopRegAct) {
        connect(macroStopRegAct, &QAction::triggered, [w, this]{ stopRecording(w); });
    } else {
        Q_ASSERT(false);
        auto s = stop_(w);
        connect(s, &QShortcut::activated, [w, this]{ stopRecording(w); });
    }

    if (macroPlaybackAct) {
        connect(macroPlaybackAct, &QAction::triggered, [w, this]{ Playback(w); });
    } else {
        Q_ASSERT(false);
        auto s = play_(w);
        connect(s, &QShortcut::activated, [w, this]{ Playback(w); });
    }
}

void KeyboardMacros::setupMenu(QMenu *menu, bool sepBefore)
{
    qDebug() << "setupMenu" << menu;

    macroStartRegAct = new QAction(tr("Macro Start Recording"), this);
    macroStartRegAct->setShortcut(start());
    macroStartRegAct->setStatusTip(tr("Keyboard Macro: Start Recording"));

    macroStopRegAct = new QAction(tr("Macro Stop Recording"), this);
    macroStopRegAct->setShortcut(stop());
    macroStopRegAct->setStatusTip(tr("Keyboard Macro: Stop Recording"));

    macroPlaybackAct = new QAction(tr("Macro Playback"), this);
    macroPlaybackAct->setShortcut(play());
    macroPlaybackAct->setStatusTip(tr("Keyboard Macro: Run Last Recorded"));

    /*
    macroSelectAct = new QAction(tr("Macro Select"), this);
    macroSelectAct->setStatusTip(tr("Select and run a Keyboard Macro Registered"));
    connect(macroSelectAct, SIGNAL(triggered()), this, SLOT(macroSelect()));
    */

    if (sepBefore)
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
        macros[lastRecorded].append(e2k(*e));
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

bool KeyboardMacros::eventFilter(QObject *obj, QEvent *event)
{
    qDebug() << "KeyboardMacros::eventFilter" << obj << event;
    switch (event->type()) {
    case QEvent::KeyPress:
    case QEvent::KeyRelease:
    case QEvent::ShortcutOverride:
        storeEvent(static_cast<QKeyEvent *>(event));
        break;
    default:
        break;
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

        emit feedback(tr("Keyboard Macro: Start registering '%1'").arg(currName()));
    }
    else
        emit feedback(tr("Keyboard Macro: Already registering"));
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

        for (const keystroke& k: macros[defaultName()]) {
            auto e = k2e(k);
            QCoreApplication::sendEvent(ed, &e);
        }

        status = idle;
        emit playbackCompleted();
    }
    else
        emit feedback(tr("Keyboard Macro: Currently recording, cannot playback"));
}
