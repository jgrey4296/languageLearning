﻿// Upgrade NOTE: replaced '_World2Object' with 'unity_WorldToObject'

Shader "Custom/VertMovement" {
	Properties {
		_Amnt ("Amount", float) = 1.0
	}

	SubShader {
		Pass {
			CGPROGRAM
			#include "UnityCG.cginc"
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag

			float4 _LightColor0;
			float _Amnt;

			struct vsIn {
				float4 position : POSITION;
				float3 normal : NORMAL;
				float2 uv : TEXCOORD0;
			};

			struct vsOut {
				float4 position : SV_POSITION;
				float3 normal : NORMAL;
				float2 uv : TEXCOORD0;
			};

			vsOut vert(vsIn v){
				vsOut o;
				o.normal = normalize(mul(float4(v.normal, 0.0), unity_WorldToObject));
				o.uv = v.uv;
				o.position = UnityObjectToClipPos(v.position);
				o.position += float4(o.normal * ((1 + _SinTime[1]) * _Amnt),0);
				return o;
			}

			float4 frag(vsOut psIn) : SV_TARGET {
				float4 ambientLight = UNITY_LIGHTMODEL_AMBIENT;

				float4 LightDirection = normalize(_WorldSpaceLightPos0);
				float4 diffuseTerm = saturate(dot(LightDirection, psIn.normal));
				float4 diffuseLight = diffuseTerm * _LightColor0;

				float csin = _SinTime[0] + _SinTime[1] + _SinTime[2];

				//float4 tex = tex2D(_MainTexture, sin(20*(psIn.uv + csin)));
				return ambientLight + diffuseLight;
			}
			ENDCG
		}
	}
}
