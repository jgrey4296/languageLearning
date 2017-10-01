using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;

[CustomEditor(typeof(JGLine))]
public class LineInspector : Editor {

	private void OnSceneGUI () {
		//The line being inspected
		JGLine line = target as JGLine;
		Transform lineTransform = line.transform;
		Quaternion lineRotation = Tools.pivotRotation == PivotRotation.Local ? lineTransform.rotation : Quaternion.identity;

		Handles.color = Color.white;
		//Transform local points to world points
		Vector3 p0 = lineTransform.TransformPoint (line.p0);
		Vector3 p1 = lineTransform.TransformPoint (line.p1);
		
		Handles.DrawLine (p0, p1);
		Handles.DoPositionHandle (p0, lineRotation);
		Handles.DoPositionHandle (p1, lineRotation);

		//Enable updating the line based of editor changes
		EditorGUI.BeginChangeCheck ();
		p0 = Handles.DoPositionHandle (p0, lineRotation);
		if (EditorGUI.EndChangeCheck ()) {
			//Add undo capability
			Undo.RecordObject (line, "Move Point");
			EditorUtility.SetDirty (line);
			//Transform world 
			line.p0 = lineTransform.InverseTransformPoint (p0);
		}

		EditorGUI.BeginChangeCheck ();
		p1 = Handles.DoPositionHandle (p1, lineRotation);
		if (EditorGUI.EndChangeCheck ()) {
			Undo.RecordObject (line, "Move Point");
			EditorUtility.SetDirty (line);
			line.p1 = lineTransform.InverseTransformPoint (p1);
		}
	}

}
