﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;

[CustomEditor(typeof(JGBezier))]
public class BezierInspector : Editor {

	private JGBezier curve;
	private Transform cTransform;
	private Quaternion cRotation;
	private const int lineSteps = 10;
	private const float directionScale = 0.5f;

	private void OnSceneGUI () {
		curve = target as JGBezier;
		cTransform = curve.transform;
		cRotation = Tools.pivotRotation == PivotRotation.Local ? cTransform.rotation : Quaternion.identity;

		Vector3 p0 = ShowPoint (0);
		Vector3 p1 = ShowPoint (1);
		Vector3 p2 = ShowPoint (2);
		Vector3 p3 = ShowPoint (3);

		Handles.color = Color.grey;
		Handles.DrawLine (p0, p1);
		Handles.DrawLine (p1, p2);
		Handles.DrawLine (p2, p3);

		ShowDirections ();
		Handles.DrawBezier (p0, p3, p1, p2, Color.white, null, 2f);

		/* not needed because of Handles.DrawBezier
		Handles.color = Color.green;
		Vector3 lineStart = curve.GetPoint (0f);
		Handles.DrawLine (lineStart, lineStart + curve.GetDirection (0f));
		for (int i = 1; i <= lineSteps; i++) {
			Handles.color = Color.red;
			Vector3 lineEnd = curve.GetPoint (i / (float)lineSteps);
			Handles.DrawLine (lineStart, lineEnd);
			Handles.color = Color.green;
			Handles.DrawLine (lineEnd, lineEnd + curve.GetDirection (i / (float)lineSteps));
			lineStart = lineEnd;
		}
		*/

	}

	//get s a value while registering for changes
	private Vector3 ShowPoint (int index){
		Vector3 point = cTransform.TransformPoint (curve.points [index]);
		EditorGUI.BeginChangeCheck ();
		point = Handles.DoPositionHandle (point, cRotation);
		if (EditorGUI.EndChangeCheck ()) {
			Undo.RecordObject (curve, "Move Point");
			EditorUtility.SetDirty (curve);
			curve.points [index] = cTransform.InverseTransformPoint (point);
		}
		return point;
	}

	private void ShowDirections(){
		Handles.color = Color.green;
		Vector3 point = curve.GetPoint (0f);
		Handles.DrawLine (point, point + curve.GetDirection (0f) * directionScale);
		for (int i = 1; i <= lineSteps; i++) {
			point = curve.GetPoint(i / (float)lineSteps);
			Handles.DrawLine(point, point + curve.GetDirection( i / (float) lineSteps) * directionScale);
		}
	}
}
