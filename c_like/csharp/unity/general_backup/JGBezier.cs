﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class JGBezier : MonoBehaviour {
	private static class Bezier {
		public static Vector3 GetPoint (Vector3 p0, Vector3 p1, Vector3 p2, float t){
			//Vector3 firstLerp = Vector3.Lerp (p0, p1, t);
			//Vector3 secondLerp = Vector3.Lerp (p1, p2, t);
			//return Vector3.Lerp (firstLerp, secondLerp, t);
			t = Mathf.Clamp01(t);
			float oneMinusT = 1f - t;
			return oneMinusT * oneMinusT * p0 +
			2f * oneMinusT * t * p1 +
			t * t * p2;

		}

		public static Vector3 GetFirstDerivative (Vector3 p0, Vector3 p1, Vector3 p2, float t){
			return 2f * (1f - t) * (p1 - p0) +
			2f * t * (p2 - p1);
		}

		public static Vector3 GetPoint3 (Vector3 p0, Vector3 p1, Vector3 p2, Vector3 p3, float t){
			t = Mathf.Clamp01 (t);
			float oneMinusT = 1f - t;
			return oneMinusT * oneMinusT * oneMinusT * p0 +
			3f * oneMinusT * oneMinusT * t * p1 +
			3f * oneMinusT * t * t * p2 +
			t * t * t * p3;
		}

		public static Vector3 GetFirstDerivative3 (Vector3 p0, Vector3 p1, Vector3 p2, Vector3 p3, float t){
			t = Mathf.Clamp01 (t);
			float oneMinusT = 1f - t;
			return 3f * oneMinusT * oneMinusT * (p1 - p0) +
			6f * oneMinusT * t * (p2 - p1) +
			3f * t * t * (p3 - p2);
		}

	}


	public Vector3[] points;

	public void Reset () {
		points = new Vector3[] {
			new Vector3 (1f, 0f, 0f),
			new Vector3 (2f, 0f, 0f),
			new Vector3 (3f, 0f, 0f),
			new Vector3 (4f, 0f, 0f)
		};
	}

	public Vector3 GetPoint (float t) {
		return transform.TransformPoint (Bezier.GetPoint3 (points [0], points [1], points [2], points[3], t));
	}

	public Vector3 GetVelocity( float t){
		return transform.TransformPoint(Bezier.GetFirstDerivative3(points[0], points[1], points[2], points[3], t)) - 
			transform.position;
	}

	public Vector3 GetDirection (float t){
		return GetVelocity (t).normalized;
	}

}

