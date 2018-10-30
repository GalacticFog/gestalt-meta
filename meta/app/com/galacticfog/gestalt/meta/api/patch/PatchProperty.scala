package com.galacticfog.gestalt.meta.api.patch


import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import scala.util.{Try,Success,Failure}
import java.util.UUID
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.patch._
import controllers.util.TypeMethods


/**
 * PatchDocument 'wrapper' for PATCH operations against GestaltTypeProperty objects.
 */
object PatchProperty {
  def applyPatch(r: GestaltTypeProperty, patch: PatchDocument): Try[JsValue] = {
    /**
     * Convert Property to JSON - apply patch to whole document
     */
     
    val newops = normalizeOps(patch.ops.toList, r)
    val patchUpdate = PatchDocument(newops: _*)

    /* 
     * This next block just extracts the properties map from the resource
     * and parses them to valid JSON, ensuring we have a valid JSON rep of the property
     */
    val json = {
      val props = r.properties.getOrElse(Map())
      val jprops = props map { case (k, v) => (k, Json.parse(v)) }      
      Json.parse(
        Json.stringify(Json.toJson(r))).as[JsObject] ++
        Json.obj("properties" -> Json.toJson(jprops))
    }
    patchUpdate.applyPatch(json)
  }
  
  
  /**
   * Translate replace op values where necessary. The values of several resource-property 
   * attributes is stored differently from how it is given in a PATCH operation. For instance, 
   * `data_type` is given as a simple string (i.e. string, int), but it is stored as
   * a UUID.
   * 
   * TODO: This function also translates the path of each op. This should not be necessary
   * and will be fixed in an upcoming build.
   * https://gitlab.com/galacticfog/gestalt-meta/issues/393
   */
  private[patch] def normalizeOps(ops: Seq[PatchOp], prop: GestaltTypeProperty): Seq[PatchOp] = {
    
    def loop(nops: Seq[PatchOp], acc: Seq[PatchOp]): Seq[PatchOp] = nops match {
      case Nil => acc
      case op :: t => {
        
        validateOp(op)
        
        op.path.drop(1) match {
          case "data_type"         => loop(t, acc ++ updateDatatype(op, ops, prop)) 
          case "visibility_type"   => loop(t, updateVisibility(op) +: acc)
          case "requirement_type"  => loop(t, updateRequirement(op) +: acc)
          case "refers_to"         => loop(t, updateRefersTo(op, ops, prop) +: acc)
          case _                   => loop(t, op +: acc)
        }
      }
    }
    loop(ops, Seq())
  }  

  
  private[patch] def updateDatatype(currentOp: PatchOp, allOps: Seq[PatchOp], prop: GestaltTypeProperty) = {

    val newDatatype = opvalue(currentOp)
    val oldReference = referenceType(prop.datatype)// is old datatype a reference?
    val newReference = referenceType(newDatatype)      // is new datatype a reference?
    val refpath = "/refers_to"
    
    /*
     * If the new type is a reference type but the old type was not, there won't be a
     * `refers_to` value set - so the user must have supplied one in the current PatchOp
     * set, or the entire request is invalid.
     * NOTE: We don't care about the case where both old and new are references. The user
     * may want to keep the same refers_to type, but change the datatype 
     * (i.e. resource::uuid -> resource::uuid::link)
     */
    if (newReference && !oldReference) {
      if (!allOps.exists(_.path == refpath)) {
        throw new BadRequestException(
            s"DataType '${newDatatype}' is a reference datatype. You must supply a value for '/refers_to' with this datatype.")
      }
    }
    
    /*
     * If the old type was a reference but the new one is not, the refers_to property must
     * be removed. We check if the user supplied a 'remove' op, and inject one if they didn't.
     */
    val removeOp = {
      if (oldReference && !newReference) {
        if (!allOps.exists(o => o.path == refpath && o.op == "replace")) {
          // No 'replace' op for 'refers_to' add one.
          Some(PatchOp.Remove("/refersTo"))
        } else None
      } else None
    }
    val updatedDatatypeOp = swapValue(currentOp)(DataType.id).copy(path = "/datatype")

    if (removeOp.nonEmpty) Seq(removeOp.get, updatedDatatypeOp)
    else Seq(updatedDatatypeOp)
  }
  
  private[patch] def updateRefersTo(op: PatchOp, allOps: Seq[PatchOp], prop: GestaltTypeProperty) = {
    if (op.path != "/refers_to") 
      throw new RuntimeException(s"Bad path. found: '${op.path}'. This is a bug.")
    
    if (op.op == "remove") {
      /*
       * If caller is trying to remove /refers_to the there must be a corresponding
       * op to change data_type to a non-reference type.
       */
      allOps.find(o => o.path == "/data_type" && o.op == "replace" && !referenceType(opvalue(o))) getOrElse {
        throw new BadRequestException(s"You cannot remove /refers_to when data_type is a reference")
      }
      op
      
    } else {
    
      /*
       * TODO: refers_to must only be present if the data_type is a reference.
       * The caller is either setting a reference data_type in the current op set
       * or the prop data_type must already be set to a reference type.
       */
      
      val typeName = opvalue(op)
      val referenceTypeId = TypeMethods.typeId(typeName) getOrElse {
        throw new BadRequestException(s"Type with name '$typeName' not found")
      }
      (swapValue(op)(_ => referenceTypeId)).copy(path = "/refersTo")
    }
  }  
  
  private[patch] def updateVisibility(op: PatchOp): PatchOp = {
    swapValue(op)(VisibilityType.id).copy(path = "/visibilityType")
  }
  
  private[patch] def updateRequirement(op: PatchOp): PatchOp = {
    swapValue(op)(RequirementType.id).copy(path = "/requirementType")
  }
  
  /**
   * Ensure there are no 'add' or 'remove' ops against core property attributes.
   * The resource-property schema is closed, so it is not possible to add or remove attributes.
   * It is possible to add or remove 'sub-properties' (i.e. anything under `/properties`), so
   * both 'add' and 'remove' are allowed if op.path begins with `/properties`.
   */
  private[patch] def validateOp(op: PatchOp): PatchOp = {
    if (Seq("add", "remove").contains(op.op)) {
      if (!op.path.contains("/properties") && op.path != "/refers_to") {
        throw new BadRequestException(
            s"Bad op [${op.op}, ${op.path}]. Property is a closed schema - it is not possible to 'add' or 'remove' attributes.")
      } else op
    } else op      
  }
  
  /**
   * Use the provided lookup function to switch out the op.value. 
   */
  private[patch] def swapValue[A](op: PatchOp)(f: String => A): PatchOp = Try {
    
    f(op.value.get.as[String])
    
  } match {
    case Failure(e) => 
      throw new BadRequestException(s"Bad value for Op [${op.op}, ${op.path}]: ${e.getMessage}")
    case Success(v) => {
      op.copy(value = Some(JsString(v.toString)))
    }
  }
    
  private[patch] def referenceType(typeName: String): Boolean = 
    typeName.startsWith("resource::")
    
  private[patch] def referenceType(datatypeId: UUID): Boolean = 
    referenceType(DataType.name(datatypeId))
  
  private[patch] def opvalue(op: PatchOp): String = op.value.get.as[String]
  
}