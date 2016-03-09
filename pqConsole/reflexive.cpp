/*
    pqConsole    : interfacing SWI-Prolog and Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

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

#define PROLOG_MODULE "pqConsole"
#include "PREDICATE.h"
#include "pqConsole.h"
#include "pqMeta.h"

#include <QStack>
#include <QDebug>
#include <QMetaObject>
#include <QMetaType>
#include <QVarLengthArray>
#include <QDate>
#include <QTime>
#include <QDateTime>
#include <QPixmap>

//! collect all registered Qt types
PREDICATE(types, 1) {
    PlTail l(PL_A1);
    for (int t = 0; t < (1 << 20); ++t)
        if (QMetaType::isRegistered(t)) {
            const char* n = QMetaType::typeName(t);
            if (n && *n)
                l.append(n);
        }
    l.close();
    return TRUE;
}

structure2(pqObj)

//! create a meta-instantiable Qt object
PREDICATE(create, 2) {
    VP obj = 0;
    QString name = t2w(PL_A1);
    long type = QMetaType::type(name.toUtf8());
    if (type)
        pqConsole::gui_run([&](){
           obj = QMetaType::create(type);  // calls default constructor here
        });
    if (obj)
        return PL_A2 = pqObj(type, obj);
    throw PlException(A(QString("create '%1' failed").arg(name)));
}

static QVariant C2V(PlTerm pl, int match = 0);
static QVariant F2V(PlTerm pl);
static QVariant T2V(PlTerm pl, int match = 0);

static QVariant C2V(PlTerm pl, int match) {
    switch (pl.type()) {
    case PL_INTEGER:
        if (match) {
            if (match == QMetaType::VoidStar ||
                match == QMetaType::QObjectStar ||
                match == qMetaTypeId<QWidget*>() ||
                match >= QMetaType::User) {
                    VP p = pl;
                    return QVariant(match, &p);
                }
        }
        return QVariant(qlonglong(long(pl)));
    case PL_FLOAT:
        return QVariant(double(pl));
    case PL_ATOM:
        return QVariant(t2w(pl));

    case PL_TERM:
        return F2V(pl);
    }
    return QVariant();
}
static QVariant F2V(PlTerm pl) {
    Q_ASSERT(pl.type() == PL_TERM);

    int ty = QMetaType::type(pl.name()),
        ar = pl.arity();

    switch (ty) {

    case QMetaType::QSize:
        return QSize(pl[1], pl[2]);

    case QMetaType::QSizeF:
        return QSizeF(pl[1], pl[2]);

    case QMetaType::QDate:
        return QDate(pl[1], pl[2], pl[3]);

    case QMetaType::QTime:
        return ar == 2 ? QTime(pl[1], pl[2]) :
               ar == 3 ? QTime(pl[1], pl[2], pl[3]) :
                         QTime(pl[1], pl[2], pl[3], pl[4]);

    case QMetaType::QDateTime:
        return QDateTime(T2V(pl[1]).toDate(), T2V(pl[2]).toTime());

    case QMetaType::QUrl:   // TBD
        return QUrl(T2V(pl[1]).toString());

    // TBD QLocale();

    case QMetaType::QRect:
        if (ar == 2) {
            QVariant a[2] =  { T2V(pl[1]), T2V(pl[2]) };
            if (a[0].type() == QVariant::Point && a[1].type() == QVariant::Point)
                return QRect(a[0].toPoint(), a[1].toPoint());
            if (a[0].type() == QVariant::Point && a[1].type() == QVariant::Size)
                return QRect(a[0].toPoint(), a[1].toSize());
            break;
        }
        return QRect(pl[1], pl[2], pl[3], pl[4]);

    case QMetaType::QRectF:
        if (ar == 2) {
            QVariant a[2] =  { T2V(pl[1]), T2V(pl[2]) };
            if (a[0].type() == QVariant::PointF && a[1].type() == QVariant::PointF)
                return QRectF(a[0].toPointF(), a[1].toPointF());
            if (a[0].type() == QVariant::PointF && a[1].type() == QVariant::SizeF)
                return QRectF(a[0].toPointF(), a[1].toSizeF());
            break;
        }
        return QRectF(pl[1], pl[2], pl[3], pl[4]);

    case QMetaType::QLine:
        return ar == 2 ? QLine(T2V(pl[1]).toPoint(), T2V(pl[2]).toPoint()) :
                         QLine(pl[1], pl[2], pl[3], pl[4]);

    case QMetaType::QLineF:
        return ar == 2 ? QLineF(T2V(pl[1]).toPointF(), T2V(pl[2]).toPointF()) :
                         QLineF(pl[1], pl[2], pl[3], pl[4]);

    case QMetaType::QPoint:
        return QPoint(pl[1], pl[2]);

    case QMetaType::QPointF:
        return QPointF(pl[1], pl[2]);

    case QMetaType::QPolygonF: {
        QPolygonF poly;
        PlTail l(pl[1]); PlTerm pt;
        while (l.next(pt))
            poly << T2V(pt).value<QPointF>();
        return poly;
    }   break;

    case QMetaType::QColor:
        if (ar == 1) {
            if (auto t2e = T2E<lqColor>(pl[1], "globalColor"))
                return QColor(static_cast<Qt::GlobalColor>(t2e.second));
            return QColor(t2w(pl[1]));
        }
        if (ar == 4)
            return QColor(int(pl[1]),int(pl[2]),int(pl[3]),int(pl[4]));
        break;

    case QMetaType::QPen:
        if (ar == 1)
            if (auto t2e = T2E<lqPen>(pl[1], "penStyle"))
                return QPen(static_cast<Qt::PenStyle>(t2e.second));
        break;

    case QMetaType::QBrush:
        if (ar == 1) {
            if (auto t2e = T2E<lqBrush>(pl[1], "brushStyle"))
                return QBrush(static_cast<Qt::BrushStyle>(t2e.second));
            if (auto e = T2ET<lqColor, Qt::GlobalColor>(pl[1], "globalColor"))
                return QBrush(e.second);
            /*
            if (auto t2e = T2E<lqColor>(pl[1], "globalColor"))
                return QBrush(static_cast<Qt::GlobalColor>(t2e.second));*/

            QVariant c = F2V(pl[1]);
            if (c.type() == QVariant::Color)
                return QBrush(c.value<QColor>());
            if (c.type() == QVariant::Pixmap)
                return QBrush(c.value<QPixmap>());
        }
        break;

    case QMetaType::QFont:
        if (ar == 1)
            return QFont(t2w(pl[1]));
        if (ar == 2)
            return QFont(t2w(pl[1]), int(pl[2]));
        if (ar == 3)
            return QFont(t2w(pl[1]), int(pl[2]), int(pl[3]));
        if (ar == 4)
            return QFont(t2w(pl[1]), int(pl[2]), int(pl[3]), bool(int(pl[4])));
        break;

    case QMetaType::QPixmap:
        if (ar == 1)
            return QPixmap(t2w(pl[1]));
    }

    return QVariant();
}

//! term to QVariant
static QVariant T2V(PlTerm pl, int match) {
    QVariant v = C2V(pl, match);
    if (!v.isValid())
        throw PlException(A(QString("cannot convert PlTerm '%1' to QVariant").arg(t2w(pl))));
    return v;
}

/** invoke(Object, Member, Args, Retv)
 *  note: pointers should be registered to safely exchange them
 */
PREDICATE(invoke, 4) {
    T ttype, tptr;
    PL_A1 = pqObj(ttype, tptr);
    QObject *obj = pq_cast<QObject>(tptr);
    if (obj) {
        QString Member = t2w(PL_A2);
        // iterate superClasses
        for (const QMetaObject *meta = obj->metaObject(); meta; meta = meta->superClass()) {
            for (int i = 0; i < meta->methodCount(); ++i) {
                const QMetaMethod &m = meta->method(i);
                if (m.methodType() == m.Method && m.access() == m.Public) {
                    QString sig = m.methodSignature();
                    if (sig.left(sig.indexOf('(')) == Member) {

                        auto const &pl = m.parameterTypes();
                        if (pl.size() > 9)
                            throw PlException("pqConsole::invoke unsupported call (max 9 arguments)");

                        // scan the argument list
                        L Args(PL_A3); T Arg;
                        // converted values buffer
                        QVariantList vl;
                        int ipar = 0;
                        for ( ; Args.next(Arg); ++ipar) {
                            if (ipar == pl.size())
                                throw PlException("argument list count mismatch (too much arguments)");
                            // match variant type
                            vl << T2V(Arg, m.parameterType(ipar));
                        }

                        /* if (ipar < m.parameterTypes().size())
                            throw PlException("argument list count mismatch (miss arguments)");
                        */

                        // optional return value
                        int trv = m.returnType();
                        QVariant rv(trv, 0);
                        QGenericReturnArgument ra(rv.typeName(), rv.data());

                        if (trv == QMetaType::Void)
                            trv = 0;    // was 0 in Qt 4

                        bool rc = false;
                        pqConsole::gui_run([&]() {

                            // fill missing arguments (instead of commented exception above)
                            for ( ; ipar < pl.size(); ipar++)
                                vl << QVariant(m.parameterType(ipar), 0);

                            QList<QGenericArgument> va;
                            for (auto &v: vl)
                                va << QGenericArgument(v.typeName(), v.data());

                            #define _0 va[0]
                            #define _1 va[1]
                            #define _2 va[2]
                            #define _3 va[3]
                            #define _4 va[4]
                            #define _5 va[5]
                            #define _6 va[6]
                            #define _7 va[7]
                            #define _8 va[8]

                            switch (pl.size()) {
                            case 0:
                                rc = trv ? m.invoke(obj, ra) : m.invoke(obj);
                                break;
                            case 1:
                                rc = trv ? m.invoke(obj, ra, _0) : m.invoke(obj, _0);
                                break;
                            case 2:
                                rc = trv ? m.invoke(obj, ra, _0,_1) : m.invoke(obj, _0,_1);
                                break;
                            case 3:
                                rc = trv ? m.invoke(obj, ra, _0,_1,_2) : m.invoke(obj, _0,_1,_2);
                                break;
                            case 4:
                                rc = trv ? m.invoke(obj, ra, _0,_1,_2,_3) : m.invoke(obj, _0,_1,_2,_3);
                                break;
                            case 5:
                                rc = trv ? m.invoke(obj, ra, _0,_1,_2,_3,_4) : m.invoke(obj, _0,_1,_2,_3,_4);
                                break;
                            case 6:
                                rc = trv ? m.invoke(obj, ra, _0,_1,_2,_3,_4,_5) : m.invoke(obj, _0,_1,_2,_3,_4,_5);
                                break;
                            case 7:
                                rc = trv ? m.invoke(obj, ra, _0,_1,_2,_3,_4,_5,_6) : m.invoke(obj, _0,_1,_2,_3,_4,_5,_6);
                                break;
                            case 8:
                                rc = trv ? m.invoke(obj, ra, _0,_1,_2,_3,_4,_5,_6,_7) : m.invoke(obj, _0,_1,_2,_3,_4,_5,_6,_7);
                                break;
                            case 9:
                                rc = trv ? m.invoke(obj, ra, _0,_1,_2,_3,_4,_5,_6,_7,_8) : m.invoke(obj, _0,_1,_2,_3,_4,_5,_6,_7,_8);
                                break;
                            }
                        });

                        if (rc && trv) {
                            // unify (some) return value
                            switch (trv) {
                            case QMetaType::Int:
                                PL_A4 = rv.toInt();
                                break;
                            case QMetaType::UInt:
                                PL_A4 = static_cast<int>(rv.toUInt());
                                break;
                            case QMetaType::LongLong:
                                PL_A4 = static_cast<int>(rv.toLongLong());
                                break;
                            case QMetaType::ULongLong:
                                PL_A4 = static_cast<int>(rv.toULongLong());
                                break;
                            case QMetaType::VoidStar:
                                PL_A4 = rv.value<void*>();
                                break;
                            default:
                                if (QMetaType::typeFlags(trv) & QMetaType::PointerToQObject)
                                    PL_A4 = rv.value<QObject*>();
                                else if (QMetaType::typeFlags(trv) & QMetaType::MovableType)
                                    PL_A4 = rv.value<void*>();
                                else
                                    throw PlException("pqConsole::invoke unsupported return type");
                            }
                        }

                        return rc;
                    }
                }
            }
        }
    }
    throw PlException("pqConsole::invoke failed");
}

/** property(Object, Property, Value)
 *  read/write a property by name
 */
PREDICATE(property, 3) {
    T ttype, tptr;
    PL_A1 = pqObj(ttype, tptr);
    QObject *obj = pq_cast<QObject>(tptr);
    if (obj) {
        // from actual class up to QObject
        for (const QMetaObject *meta = obj->metaObject(); meta; meta = meta->superClass()) {
            int ip = meta->indexOfProperty(t2w(PL_A2).toUtf8());
            if (ip >= 0) {
                QMetaProperty p = meta->property(ip);
                QVariant v;
                bool isvar = PL_A3.type() == PL_VARIABLE, rc = false;
                if (!isvar)
                    v = T2V(PL_A3);
                pqConsole::gui_run([&]() {
                    if (isvar) {
                        v = p.read(obj);
                        rc = true;
                    }
                    else {
                        rc = p.write(obj, v);
                    }
                });

                return rc;
            }
        }
    }
    throw PlException("pqConsole::property failed");
}

/** unify a property of QObject:
 *  allows read/write of simple atomic values
 */
QString pqConsole::unify(const QMetaProperty& p, QObject *o, PlTerm v) {

    #define OK return QString()
    QVariant V;

    switch (v.type()) {

    case PL_VARIABLE: {

        gui_run([&]() { V = p.read(o); });

        switch (p.type()) {
        case QVariant::Bool:
            v = V.toBool() ? A("true") : A("false");
            OK;
        case QVariant::Int:
            if (p.isEnumType()) {
                Q_ASSERT(!p.isFlagType());  // TBD
                QMetaEnum e = p.enumerator();
                if (CCP key = e.valueToKey(V.toInt())) {
                    v = A(key);
                    OK;
                }
            }
            v = long(V.toInt());
            OK;
        case QVariant::UInt:
            v = long(V.toUInt());
            OK;
        case QVariant::String:
            v = A(V.toString());
            OK;
        default:
            break;
        }
    }   break;

    case PL_INTEGER:
        switch (p.type()) {
        case QVariant::Int:
        case QVariant::UInt:
            V = qint32(v);
            break;
        default:
            break;
        }
        break;

    case PL_ATOM:
        switch (p.type()) {
        case QVariant::String:
            V = t2w(v);
            break;
        case QVariant::Int:
            if (p.isEnumType()) {
                Q_ASSERT(!p.isFlagType());  // TBD
                int i = p.enumerator().keyToValue(v);
                if (i != -1)
                    V = i;
            }
        default:
            break;
        }
        break;

    case PL_FLOAT:
        switch (p.type()) {
        case QVariant::Double:
            V = double(v);
            break;
        default:
            break;
        }
        break;

    default:
        break;
    }

    if (V.isValid()) {
        bool retok = false;
        gui_run([&]() { if (p.write(o, V)) retok = true; });
        if (retok)
            OK;
    }

    return o->tr("property %1: type mismatch").arg(p.name());
}

/** unify a property of QObject, seek by name:
 *  allows read/write of basic atomic values (note: enums are symbolics)
 */
QString pqConsole::unify(CCP name, QObject *o, PlTerm v) {
    int pid = o->metaObject()->indexOfProperty(name);
    if (pid >= 0)
        return unify(o->metaObject()->property(pid), o, v);
    return o->tr("property %1: not found").arg(name);
}
