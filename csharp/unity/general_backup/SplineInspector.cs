using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEditor;

[CustomEditor(typeof(JGSpline))]
public class SplineInspector : Editor {

	private JGSpline curve;
	private Transform cTransform;
	private Quaternion cRotation;
	private const int lineSteps = 10;
	private const float directionScale = 0.5f;
	private const int stepsPerCurve = 10;
	private const float handleSize = 0.04f;
	private const float pickSize = 0.06f;
	private int selectedIndex = -1;

	public override void OnInspectorGUI (){
		DrawDefaultInspector ();
		curve = target as JGSpline;
		if (GUILayout.Button ("Add Curve")) {
			Undo.RecordObject (curve, "Add Curve");
			curve.AddCurve ();
			EditorUtility.SetDirty (curve);
		}
	}

	private void OnSceneGUI () {
		curve = target as JGSpline;
		cTransform = curve.transform;
		cRotation = Tools.pivotRotation == PivotRotation.Local ? cTransform.rotation : Quaternion.identity;

		//Loop 
		Vector3 p0 = ShowPoint (0);
		for (int i = 1; i < curve.points.Length; i += 3) {
			Vector3 p1 = ShowPoint (i);
			Vector3 p2 = ShowPoint (i + 1);
			Vector3 p3 = ShowPoint (i + 2);

			Handles.color = Color.grey;
			Handles.DrawLine (p0, p1);
			Handles.DrawLine (p2, p3);
			
			Handles.DrawBezier (p0, p3, p1, p2, Color.white, null, 2f);
			p0 = p3;
		}

		ShowDirections ();
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
		float size = HandleUtility.GetHandleSize (point);
		Handles.color = Color.white;
		if (Handles.Button (point, cRotation, size * handleSize, size * pickSize, Handles.DotHandleCap)) {
			selectedIndex = index;
		}
		if (selectedIndex == index) {
			EditorGUI.BeginChangeCheck ();
			point = Handles.DoPositionHandle (point, cRotation);
			if (EditorGUI.EndChangeCheck ()) {
				Undo.RecordObject (curve, "Move Point");
				EditorUtility.SetDirty (curve);
				curve.points [index] = cTransform.InverseTransformPoint (point);
			}
		}
		return point;
	}

	private void ShowDirections(){
		Handles.color = Color.green;
		Vector3 point = curve.GetPoint (0f);
		Handles.DrawLine (point, point + curve.GetDirection (0f) * directionScale);
		int steps = stepsPerCurve * curve.CurveCount;
		for (int i = 1; i <= steps; i++) {
			point = curve.GetPoint(i / (float)steps);
			Handles.DrawLine(point, point + curve.GetDirection( i / (float) steps) * directionScale);
		}
	}


}
