package dotty.tools.scaladoc
package transformers

// for a particular given, the range of its inclusion in "known givens"
// should be up to and including the declared given type.
//
// but this is thwarted by the synthesised given types.
// we need to "see through" the synthesised types, but how can we identify them?
//
// it looks like we can find types which have the same TastyMemberSource
// as a given statement and presume that those are synthesised types.
// probably also check the "given_" prefix.
//
// another complication:
// traits with multiple parameters - how do we choose which types an instance
// should be attached to? e.g., CanEqual[A, B] - do we record this in both A and B?

trait GivenTrait[T] {
  def f = 20
}

// so these ones synthesise a new type which is largely useless.
given GivenTrait[Int] with {

}


given [T](using GivenTrait[T]): GivenTrait[Option[T]] with {

}

// and these ones just use the parent trait in the signature.
given GivenTrait[String] = new GivenTrait[String] { }

class Boop {

given GivenTrait[Boolean] with {

}

}



class InheritanceInformationTransformer(using DocContext) extends (Module => Module):
  override def apply(original: Module): Module =
    val subtypes = getSupertypes(original.rootPackage).groupMap(_(0))(_(1)).view.mapValues(_.distinct).toMap

    var givens = getGivens(original.rootPackage).groupMap(_._1)(_._2).view.mapValues(_.distinct).toMap

    val synthesised = getSynthesisedGivens(original)(givens.get(_))

    givens = synthesised.foldLeft(givens) { case (givens,(oldDri, realDri)) =>
      givens.get(oldDri) match {
        case Some(synthesisedInstances) =>
          val updatedInstances = synthesisedInstances ++ givens.getOrElse(realDri, Seq())
          givens + (realDri -> updatedInstances)
        case _ => givens
      }
    }

    original.updateMembers { m =>
      val edges = getEdges(m.asLink.copy(kind = bareClasslikeKind(m.kind)), subtypes)
      val st: Seq[LinkToType] = edges.map(_._1).distinct

      m.withKnownChildren(st).withNewGraphEdges(edges.toSeq)
    }

    original.updateMembers { m =>
      val links = givens.getOrElse(m.dri, Nil).map { m =>
        m.kind match {
          case Kind.Given(_, _, _) =>
            val sigProvider = translators.ScalaSignatureProvider()
            val sig = sigProvider.rawSignature(m)(m.kind)
            println(sig)
            m.asLink.copy(signature = sig.prefix ++ sig.kind ++ sig.name ++ sig.suffix)
          case _ => m.asLink
        }
      }
      // if (links.nonEmpty) {
      //   println(links)
      // }
      m.copy(knownGivenInstances = links)
    }



  private def getEdges(ltt: LinkToType, subtypes: Map[DRI, Seq[LinkToType]]): Seq[(LinkToType, LinkToType)] =
    val st: Seq[LinkToType] = subtypes.getOrElse(ltt.dri, Vector.empty)
    st.flatMap(s => Vector(s -> ltt) ++ getEdges(s, subtypes))

  private def bareClasslikeKind(kind: Kind): Kind = kind match
    case _: Kind.Trait => Kind.Trait(Nil, Nil)
    case _: Kind.Class => Kind.Class(Nil, Nil)
    case e if e.isInstanceOf[Kind.Enum] => Kind.Enum(Nil, Nil)
    case ec if ec.isInstanceOf[Kind.EnumCase] => Kind.EnumCase(Kind.Object)
    case o => o

  private def getSupertypes(c: Member): Seq[(DRI, LinkToType)] =
    val selfMapping =
      if !c.kind.isInstanceOf[Classlike] && !c.kind.isInstanceOf[Kind.EnumCase] then Nil
      else c.directParents.map(p => p.dri -> c.asLink)
    c.members.flatMap(getSupertypes) ++ selfMapping

  // map of synthesised type to real type DRIs
  private def getSynthesisedGivens(module: Module)(givens: DRI => Option[Seq[Member]]): Map[DRI, DRI] =
    var synthesised = Map[DRI, DRI]()

    module.visitMembers { m =>
      if (m.fullName.contains("_Option")) {
        println(m)
      }


      givens(m.dri) match {
        case Some(Seq(instance)) if m.name.startsWith("given_")
            && m.sources.nonEmpty && m.sources == instance.sources =>

          m.directParents match {
            case Seq(LinkToType(_, realDri, _)) =>
              // if this happens, then `m` is a synthesised given object
              // and we should replace it with its parent in the givens map.
              synthesised = synthesised + (m.dri -> realDri)
              // println("found synthesised given " + m.fullName)
              // println("old: " + m.dri)
              // println("new: " + realDri)
            case _ => ()
          }
        case _ => ()
      }
    }

    synthesised

  // to get the trait of a synthesised given instance, we will need to find
  // the type of its parent trait.
  //
  // returns an association between given type and its known instances.
  private def getGivens(c: Member): Seq[(DRI, Member)] =
    // println(c.kind)
    // println("" + c.kind + c.fullName)
    val selfMapping = c.kind match {
      case Kind.Given(_, Some(Type(_, Some(typeDri)) :: _), _) =>
        // println("" + c + " = " + sig)
        Seq(typeDri -> c)
      case _: Kind.Given =>
        println("UNKNOWN GIVEN " + c.kind)
        Nil
      case _ => Nil
    }

    // if (selfMapping.nonEmpty) {
    // println("selfmapping " + selfMapping.map{case (a,b) => (a, b.fullName)})
    //
    // }
    c.members.filter { c => c.kind match
      case Kind.Unknown | Kind.Object | Kind.RootPackage | Kind.Package | Kind.Given(_, _, _) => true
      case _ => false
    }.flatMap(getGivens) ++ selfMapping
