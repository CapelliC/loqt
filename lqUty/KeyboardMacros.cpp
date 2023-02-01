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

#include <QCoreApplication>
#include <QDebug>
#include <stdexcept>

static const char* k_macros = "KeyboardMacros";
static const char* k_name = "name";
static const char* k_strokes = "strokes";

KeyboardMacros::KeyboardMacros(QObject *parent) :
    QObject(parent),
    status(idle)
{
    lqPreferences p;

    int n = p.beginReadArray(k_macros);
    for (int i = 0; i < n; ++i) {
        p.setArrayIndex(i);
        auto name = p.value(k_name).toString();
        macro m;
        auto strokes = p.value(k_strokes).toStringList();
        for(const auto &s: strokes)
            m << s2k(s);
        /*
        QList<QKeyEvent> lkeys;
        for(const auto &e: lkeys)
            m << e2k(e);
        */
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
        p.setArrayIndex(i++);
        p.setValue(k_name, name);
        /*
        QList<QKeyEvent> l;
        foreach (const keystroke &k, macros[name])
            l.append(k2e(k));
        p.setValue(k_strokes, 1);
        */
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
    connect(startRegAct, &QAction::triggered, this, [w, this]{ startRecording(w); });
    connect(stopRegAct, &QAction::triggered, this, [w, this]{ stopRecording(w); });
    connect(playbackAct, &QAction::triggered, this, [w, this]{ Playback(w); });
}

void KeyboardMacros::setupMenu(QMenu *menu, bool sepBefore)
{
    startRegAct = new QAction(tr("Macro Start Recording"), this);
    startRegAct->setShortcut(start());
    startRegAct->setStatusTip(tr("Keyboard Macro: Start Recording"));

    stopRegAct = new QAction(tr("Macro Stop Recording"), this);
    stopRegAct->setShortcut(stop());
    stopRegAct->setStatusTip(tr("Keyboard Macro: Stop Recording"));

    playbackAct = new QAction(tr("Macro Playback"), this);
    playbackAct->setShortcut(play());
    playbackAct->setStatusTip(tr("Keyboard Macro: Run Last Recorded"));

    /*
    macroSelectAct = new QAction(tr("Macro Select"), this);
    macroSelectAct->setStatusTip(tr("Select and run a Keyboard Macro Registered"));
    connect(macroSelectAct, SIGNAL(triggered()), this, SLOT(macroSelect()));
    */

    if (sepBefore)
        menu->addSeparator();
    menu->addAction(startRegAct);
    menu->addAction(stopRegAct);
    menu->addAction(playbackAct);
    //menu->addAction(macroSelectAct);
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
    switch (event->type()) {
    case QEvent::KeyPress:
    case QEvent::KeyRelease:
    case QEvent::ShortcutOverride:
        if (status == onRecord)
            macros[lastRecorded].append(e2k(*static_cast<QKeyEvent *>(event)));
        break;
    default:
        break;
    }
    // standard event processing
    return QObject::eventFilter(obj, event);
}

void KeyboardMacros::startRecording(QWidget *ed)
{
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

#include <QTextStream>
KeyboardMacros::keystroke KeyboardMacros::s2k(const QString &x) {
    QString s(x);
    QTextStream S(&s);
    QChar c1,c2,c3,c4,c5;
    quint32
        type,
        key,
        modifiers,
        autorep,
        count;
    S >>type      >> c1 >>
        key       >> c2 >>
        modifiers >> c3 >>
        autorep   >> c4 >>
        count     >> c5;
    keystroke k {QEvent::None};
    if (S.status() == QTextStream::Ok && c1 == ',' && c5 == ',') {
        k.type      = static_cast<QKeyEvent::Type>(type);
        k.key       = key       ;
        k.modifiers = static_cast<Qt::KeyboardModifiers>(modifiers);
        k.autorep   = autorep   ;
        k.count     = count     ;
        k.text      = S.readAll();
    }
    return k;
}

/*
#include <QDataStream>
#include <QRegularExpression>
#include <QTextStream>

KeyboardMacros::keystroke KeyboardMacros::s2k(const QString &s) {
    QDataStream d;

    keystroke k {QEvent::None};
    #define cI "(\\d+)"
    #define cS "'((?:[\\']|.)*)'"
    static QRegularExpression e(cI "," cI "," cI "," cS "," cI "," cI);
    auto m = e.match(s);
    if (m.hasMatch()) {
        k.type      = static_cast<QKeyEvent::Type>(m.captured(1).toUInt());
        k.key       = m.captured(2).toInt();
        k.modifiers = static_cast<Qt::KeyboardModifiers>(m.captured(3).toUInt());
        QString t(m.captured(4));
        while (t.indexOf("\'"))
           t.replace("\'", "'");
        k.text      = t;
        k.autorep   = m.captured(5).toUInt();
        k.count     = m.captured(6).toUInt();
    }
    return k;
}
*/
/*
QString KeyboardMacros::k2s(const KeyboardMacros::keystroke& k) {
    QString s;
    {   QTextStream S(&s);
        QString t(k.text);
        t.replace("'", "\\'");
        S << static_cast<quint32>(k.type) << ','
          << k.key << ','
          << static_cast<quint32>(k.modifiers) << ",'" << t << "',"
          << static_cast<quint32>(k.autorep) << ','
          << k.count;
    }
    return s;
}
KeyboardMacros::keystroke KeyboardMacros::s2k(const QString &s) {
    keystroke k {QEvent::None};
    #define cI "(\\d+)"
    #define cS "'((?:[\\']|.)*)'"
    static QRegularExpression e(cI "," cI "," cI "," cS "," cI "," cI);
    auto m = e.match(s);
    if (m.hasMatch()) {
        k.type      = static_cast<QKeyEvent::Type>(m.captured(1).toUInt());
        k.key       = m.captured(2).toInt();
        k.modifiers = static_cast<Qt::KeyboardModifiers>(m.captured(3).toUInt());
        QString t(m.captured(4));
        while (t.indexOf("\'"))
           t.replace("\'", "'");
        k.text      = t;
        k.autorep   = m.captured(5).toUInt();
        k.count     = m.captured(6).toUInt();
    }
    return k;
}
*/
QString KeyboardMacros::k2s(const KeyboardMacros::keystroke& k) {
    QString s;
    {   QTextStream S(&s);
        S << k.type << ','
          << k.key << ','
          << k.modifiers << ','
          << k.autorep << ','
          << k.count << ','
          << k.text;
    }
    return s;
}
